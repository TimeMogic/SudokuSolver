module Tutorial9 where

-- Sudoku solver
-- Based on Bird, "Thinking Functionally with Haskell"

import Data.List (sort,nub,(\\),transpose,genericLength)
import Data.String (lines,unlines)
import Test.QuickCheck

type Row a    = [a]
type Matrix a = [Row a]
type Digit    = Char

digits :: [Digit]
digits = ['1'..'9']

blank :: Digit -> Bool
blank d = d == ' '

-- 1.
group :: [a] -> [[a]]
group = groupBy 3

groupBy :: Int -> [a] -> [[a]]
groupBy _ [] = []
groupBy k xs = take k xs : groupBy k (drop k xs)

-- 2.
intersperse :: a -> [a] -> [a]
intersperse i [] = [i]
intersperse i (x:xs) = i : x : intersperse i xs

-- 3.
showRow :: String -> String
showRow = concat.intersperse "|".group

-- 4.
showGrid :: Matrix Digit -> [String]
showGrid = concat.intersperse ["-------------"].group.map showRow

-- 5.
put :: Matrix Digit -> IO ()
put = putStrLn.unlines.showGrid

-- 6.
showMat :: Matrix Digit -> String
showMat = fwDots.concat
        where
        fwDots [] = []
        fwDots (x:xs)   | x == ' ' = '.' : fwDots xs
                        | otherwise = x : fwDots xs


readMat :: String -> Matrix Digit
readMat = groupBy 9.noDots
        where
        noDots [] = []
        noDots (x:xs)   | x == '.' = ' ' : noDots xs
                        | otherwise = x : noDots xs

-- 7.
choices :: Matrix Digit -> Matrix [Digit]
choices = map cho
        where
        cho [] = []
        cho (x:xs)      | x == ' ' = "123456789" : cho xs
                        | otherwise = [x] : cho xs

-- 8.
cp :: [[a]] -> [[a]]
cp []       = [[]]
cp (xs:xss) = [ x:ys | x <- xs, ys <- cp xss ]

prop_cp :: [[a]] -> Bool
prop_cp xss = length (cp xss) == product (map length xss)
-- slow: use `quickCheckWith (stdArgs {maxSize = 5})`

expand :: Matrix [Digit] -> [Matrix Digit]
expand []       = [[]]
expand (xs:xss) = [ x:ys | x <- cp xs, ys <- expand xss]

-- 9.
prop_expand :: Matrix [Digit] -> Bool
prop_expand m = length (expand m) == product (map length (concat m))

-- 10.
easySols :: Integer
easySols = product (map (fromIntegral . length) (concat (choices easy)))
-- 11, 12, 13.
rows, cols, boxs :: Matrix a -> Matrix a
rows x = x
cols = transpose
boxs = map ungroup.ungroup.map cols.group.map group
  where
    ungroup :: Matrix a -> [a]
    ungroup = concat

-- 14.
distinct :: Eq a => [a] -> Bool
distinct xs = length (nub xs) == length xs

-- 15.
valid :: Matrix Digit -> Bool
valid g = all distinct g && all distinct (cols g) && all distinct (boxs g)

-- 16.
simple :: Matrix Digit -> [Matrix Digit]
simple = filter valid . expand . choices

-- 17.
the :: [Digit] -> Digit
the [d] = d

pruneRow :: Row [Digit] -> Row [Digit]
pruneRow row = [rmv xs (singles row) | xs <- row]
        where
        rmv xs ys       | length xs > 1 = [x | x<-xs, x `notElem` ys]
                        | otherwise = xs
        singles xs = concat[ x | x<- xs, length x == 1]

-- 18.
pruneBy :: (Matrix [Digit] -> Matrix [Digit]) -> Matrix [Digit] -> Matrix [Digit]
pruneBy f = f . map pruneRow . f

prune :: Matrix [Digit] -> Matrix [Digit]
prune = pruneBy boxs.pruneBy cols.pruneBy rows

-- 19.
many :: Eq a => (a -> a) -> a -> a
many g x        | g x == x = x
                | otherwise = many g (g x)

-- 20.
extract :: Matrix [Digit] -> Matrix Digit
extract = map (map the)

-- 21.
solve :: Matrix Digit -> Matrix Digit
solve = extract . many prune . choices


-- ** Optional Material

-- 22.
failed :: Matrix [Digit] -> Bool
failed = any null.concat

-- 23.
solved :: Matrix [Digit] -> Bool
solved = all (\x -> length x == 1).concat

-- 24.
shortest :: Matrix [Digit] -> Int
shortest = minimum.filter (>1).map length.concat

-- 25.
expand1 :: Matrix [Digit] -> [Matrix [Digit]]
expand1 mat = [preMat ++ [preRow ++ [[d]] ++ postRow] ++ postMat | d <- ds]
        where
        p ds = length ds == shortest mat
        preMat = fst (break (any p) mat)
        row = head (snd (break (any p) mat))
        postMat = tail (snd (break (any p) mat))
        preRow = fst (break p row)
        ds = head (snd (break p row))
        postRow = tail (snd (break p row))

-- 26.
search :: Matrix Digit -> [Matrix Digit]
search mat = sol [many prune (choices mat)]
        where
        sol [] = []
        sol (m:ms) | solved m = extract m : sol ms
                   | failed m = []
                   | otherwise = sol (map (many prune) (expand1 m)) ++ sol ms



-- Example from Bird

book :: Matrix Digit
book = ["  4  57  ",
        "     94  ",
        "36      8",
        "72  6    ",
        "   4 2   ",
        "    8  93",
        "4      56",
        "  53     ",
        "  61  9  "]

-- Examples from websudoku.com

easy :: Matrix Digit
easy = ["    345  ",
        "  89   3 ",
        "3    2789",
        "2 4  6815",
        "    4    ",
        "8765  4 2",
        "7523    6",
        " 1   79  ",
        "  942    "]

medium :: Matrix Digit
medium = ["   4 6 9 ",
          "     3  5",
          "45     86",
          "6 2 74  1",
          "    9    ",
          "9  56 7 8",
          "71     64",
          "3  6     ",
          " 6 9 2   "]

hard :: Matrix Digit
hard = ["9 3  42  ",
        "4 65     ",
        "  28     ",
        "     5  4",
        " 67 4 92 ",
        "1  9     ",
        "     87  ",
        "     94 3",
        "  83  6 1"]

evil :: Matrix Digit
evil = ["  9      ",
        "384   5  ",
        "    4 3  ",
        "   1  27 ",
        "2  3 4  5",
        " 48  6   ",
        "  6 1    ",
        "  7   629",
        "     5   "]

puts :: [Matrix Digit] -> IO ()
puts = sequence_ . map put

puzzle :: Matrix Digit -> IO ()
puzzle g = put g >> puts (search g) >> putStrLn "***"

main :: IO ()
main = puzzle easy >>
       puzzle medium >>
       puzzle hard >>
       puzzle evil
