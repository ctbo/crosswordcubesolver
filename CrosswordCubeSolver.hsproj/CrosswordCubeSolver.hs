-- | Crossword Cube Solver (C) by Harald Bögeholz
module Main where

import Data.List (tails, cycle, sort, group, find, lookup, delete, intersperse, transpose)
import Data.Array.IArray
import Control.Monad
import Debug.Trace
import Data.Maybe

takeOne :: [a] -> [(a, [a])]
takeOne [] = []
takeOne (x:xs) = (x, xs) : map (\(y, ys) -> (y, x:ys)) (takeOne xs)

shifts :: [a] -> [[a]]
shifts xs = take l $ map (take l) $ tails $ cycle xs
    where l = length xs


data Orientation = Upright | TurnedLeft | TurnedRight | UpsideDown
                 | UpDown | LeftRight | Any | Invalid deriving (Eq, Ord, Enum)

instance Show Orientation where
    show Upright = "^"
    show TurnedLeft = "<"
    show TurnedRight = ">"
    show UpsideDown = "v"
    show UpDown = "|"
    show LeftRight = "-"
    show Any = " "
    show Invalid = "x"

readOrientation :: Char -> Orientation
readOrientation '^' = Upright
readOrientation '<' = TurnedLeft
readOrientation '>' = TurnedRight
readOrientation 'v' = UpsideDown
readOrientation '|' = UpDown
readOrientation '-' = LeftRight
readOrientation x = error $ "invalid orentation '" ++ [x] ++ "'"

-- | specialize
(><<) :: Orientation -> Orientation -> Orientation
Any ><< x = x
x ><< Any = x
LeftRight ><< TurnedLeft = TurnedLeft
TurnedLeft ><< LeftRight = TurnedLeft
LeftRight ><< TurnedRight = TurnedRight
TurnedRight ><< LeftRight = TurnedRight
UpDown ><< Upright = Upright
Upright ><< UpDown = Upright
UpDown ><< UpsideDown = UpsideDown
UpsideDown ><< UpDown = UpsideDown
x ><< y
    | x == y = x
    | otherwise = Invalid

type Xform = Orientation -> Orientation

turnLeft :: Xform
turnLeft Upright = TurnedLeft
turnLeft TurnedLeft = UpsideDown
turnLeft UpsideDown = TurnedRight
turnLeft TurnedRight = Upright
turnLeft UpDown = LeftRight
turnLeft LeftRight = UpDown
turnLeft x = x

turnAround :: Xform
turnAround = turnLeft . turnLeft

inverse :: Xform -> Xform
inverse f = f . f . f

turnRight :: Xform
turnRight = inverse turnLeft

type CubeLocation = [(Int, Xform)]
cubeLocations :: [CubeLocation]
cubeLocations = [ [(2,  id),         (53, turnAround)]
                , [(4,  turnLeft),   (11, id)]
                , [(6,  turnRight),  (17, id)]
                , [(8,  turnAround), (14, id)]
                , [(19, turnLeft),   (49, turnLeft)]
                , [(22, turnLeft),   (21, turnRight)]
                , [(24, turnRight),  (25, turnLeft)]
                , [(27, turnRight),  (51, turnRight)]
                , [(29, turnAround), (40, turnLeft)]
                , [(32, turnAround), (38, id)]
                , [(35, turnAround), (42, turnRight)]
                , [(44, turnAround), (47, id)]
                , [(13, id),         (7,  turnLeft),   (12, turnRight)]
                , [(15, turnRight),  (16, id),         (9,  turnAround)]
                , [(31, turnLeft),   (30, turnAround), (37, id)]
                , [(33, turnAround), (39, turnRight),  (34, turnLeft)]
                , [(46, id),         (43, turnLeft),   (28, turnLeft)]
                , [(48, turnRight),  (36, turnAround), (45, turnAround)]
                , [(54, turnAround), (3,  turnRight),  (18, turnRight)]
                , [(52, turnLeft),   (10, id),         (1, id)]
                ]

faces :: [[Int]]
faces = [ [1..9]
        , [10,11,12,19,20,21,28,29,30]
        , [13,14,15,22,23,24,31,32,33]
        , [16,17,18,25,26,27,34,35,36]
        , [37..45]
        , [46..54]
        ]

center :: Int -> Int
center x = (!!4) $ fromJust $ find (elem x) faces

centers :: [Int]
centers = map (!!4) faces

faceLines :: Orientation -> [[Int]]
faceLines Upright     = [[0,1,2], [3,4,5], [6,7,8]]
faceLines TurnedLeft  = [[6,3,0], [7,4,1], [8,5,2]]
faceLines TurnedRight = [[2,5,8], [1,4,7], [0,3,6]]
faceLines UpsideDown  = [[8,7,6], [5,4,3], [2,1,0]]
faceLines x           = error $ "can't read face; invalid orientation " ++ show x

data Label = Label { lChar :: Char
                   , lOrientation :: Orientation
                   , lId :: Int } 
           | Blank

readLabel :: (Int, String) -> Label
readLabel (i, [c, o]) = Label c (readOrientation o) i
readLabel (i, l) = error $ "Invalid label #" ++ show i ++ ": " ++ l

instance Show Label where
    show (Label c o _) = c : show o
    show Blank = ". "

rotate :: Xform -> Label -> Label
rotate f (Label c o i) = Label c (f o) i
rotate _ Blank = Blank

type Cube = Array Int Label

readCube :: String -> Cube
readCube s = if length w == 54 then listArray (1, 54) (map readLabel (zip [1..] w))
                               else error $ "Invalid file format. Cube must have 54 labels (has "++show (length w)++")."
    where w = words s

showCube :: Cube -> String
showCube cube = ilines [1,4,7] ++ llines [10,19,28] ++ ilines [37,40..52]
        where ilines = concatMap iline
              iline i = replicate 9 ' ' ++ line i 3
              llines = concatMap lline
              lline i = line i 9
              line i l = concatMap (label i) [0..l-1] ++ "\n"
              label i j = show (cube ! (i+j)) ++ " "

inversions :: Cube -> Int
inversions cube = length [() | i <- [1 .. 53]
                             , j <- [i+1 .. 54]
                             , lId (cube!i) > lId (cube!j)]

type Piece = [Label]

readPiece :: Cube -> CubeLocation -> Piece
readPiece cube faces = map rp faces
    where rp (i, f) = rotate (inverse f) $ cube!i

-- | a partially solved cube where [Piece] contains unplaced pieces
data Puzzle = Puzzle { pCube :: Cube
                     , pUnused :: [Piece]
                     }

cube2pieces :: Cube -> [Piece]
cube2pieces cube = map (readPiece cube) cubeLocations

readPuzzle :: String -> Puzzle
readPuzzle s = Puzzle centerCube pieces
    where cube = readCube s
          pieces = cube2pieces cube
          centerCube = listArray (1,54) (replicate 54 Blank) // map centerFace centers
          centerFace i = (i, rotate (const Any) $ cube!i)

fillFace :: Cube -> ((Int, Xform), Label) -> [Cube]
fillFace cube ((i, f), l) = if faceOrientation /= Invalid
                               then [cube // [(i, l'), (j, rotate (const faceOrientation) cl)]]
                               else []
    where l' = rotate f l
          j = center i
          cl = cube ! j
          faceOrientation = lOrientation cl ><< lOrientation l'

fillLocation :: Puzzle -> CubeLocation -> [Puzzle]
fillLocation (Puzzle cube pieces) faces = 
        [Puzzle cube' pieces' | (piece, pieces') <- takeOne pieces
                              , length piece == length faces
                              , piece' <- shifts piece
                              , cube' <- foldM fillFace cube (zip faces piece')
                              ]

solve :: Puzzle -> [Puzzle]
solve puzzle = foldM fillLocation puzzle cubeLocations

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

analyzePieces :: [Piece] -> Int
analyzePieces pieces = product (map factorial perms) * product turns
    where opieces = (map.map) lOrientation pieces
          normalize piece = head $ sort $ shifts piece
          npieces = map normalize opieces
          symmetries p = sum [1 | s <- shifts p, s == p]
          perms = map length $ group $ sort npieces
          turns = map symmetries npieces

testcube :: [Orientation] -> Cube
testcube os = listArray (1,54) $ map copyCenter [1..54]
    where ccube :: Cube
          ccube = array (1, 54) $ map (\(c, o) -> (c, Label 'X' o c)) $ zip centers os
          copyCenter c = ccube ! (center c)

testcubes = map testcube [[c5, c20, Upright, c26, c41, c50]  | c20 <- [Upright, TurnedLeft, TurnedRight, UpsideDown]
                                                             , c5  <- [Upright, TurnedLeft, TurnedRight, UpsideDown]
                                                             , c26 <- [Upright, TurnedLeft, TurnedRight, UpsideDown]
                                                             , c41 <- [Upright, TurnedLeft, TurnedRight, UpsideDown]
                                                             , c50 <- [Upright, TurnedLeft, TurnedRight, UpsideDown]
                                                             ]
ctfilter :: Puzzle -> Bool
ctfilter puzzle = lOrientation (cube!23) == Upright && all inplace [(14, 'C'), (22, 'C'), (24, 'T'), (32, 'T')]
    where inplace (i, c) = lChar (cube!i) == c
          cube = pCube puzzle

cycleShow :: [(Int, Int)] -> String
cycleShow [] = ""
cycleShow ((a, b):xs) 
    | a == b = cycleShow xs
    | otherwise = let (string, rest) = cycle b xs in "(" ++ show a ++ string ++ cycleShow rest
    where cycle n ps  = case lookup n ps of
                           Just m -> let (string, rest) = cycle m (delete (n,m) ps) in ("," ++ show n ++ string, rest)
                           Nothing -> (")", ps)

cube2perm :: Cube -> [(Int, Int)]
cube2perm cube = map f $ assocs cube
    where f (i, l) = (i, lId l)

cube2wordlist :: Cube -> [String]
cube2wordlist cube = sort $ concatMap face2words labels
    where labels = (map.map) (cube!) faces

face2words :: [Label] -> [String]
face2words ls = horizontal ++ vertical
    where orientation = foldr (><<) Any $ map lOrientation ls
          chars = map lChar ls
          horizontal = (map.map) (chars!!) $ faceLines orientation
          vertical = transpose horizontal

ctWordlist = ["ADD","C'T","C'T","CMD","CSS","CVS","DFS","DOS","DPI","EOF","ESC","FTP","HDD","HEX","IPC","IPS","ISA","MOV","OCR","OCT","OSI","PCI","PDF","PID","PIP","POP","PPP","PPT","RMI","RPC","RTF","SCP","SEO","SIM","TTF","XSS"]

-- | The main entry point.
main :: IO ()
main = do
    puzzleFile <- readFile "nerdcube_scrambled.cube"
    let puzzle = readPuzzle puzzleFile
{-
    let complexities = map (analyzePieces.cube2pieces) testcubes
    let m = minimum complexities
    print m
    let cs = filter (\c -> analyzePieces (cube2pieces c) == m) testcubes
    print $ length cs
    putStrLn $ showCube $ cs !! 50
-}
{-
    let solutions = solve puzzle
    putStrLn $ showCube $ pCube $ head solutions
    print $ length solutions
    let ctsolutions = filter ctfilter solutions
    --let possible = map snd $ filter fst $ zip [True,False,False,False,False,True,False,False,False,False,True,False,False,False,False,True] ctsolutions
    putStr $ concat $ intersperse "\n" $ map  (showCube . pCube ) ctsolutions
    print $ length ctsolutions
    forM_ ctsolutions (\p -> print (cube2wordlist (pCube p)))
-}
    solutionFile <- readFile "solution.cube"
    let wordlist =  cube2wordlist (readCube solutionFile)
    print wordlist
    print $ ctWordlist == wordlist
    print $ filter (`notElem` ctWordlist) wordlist
    
    