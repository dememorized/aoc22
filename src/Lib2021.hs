module Lib2021 (
    day01,
    day02,
    ) where

import Lib

day01 :: [Int] -> (Int, Int)
day01 xs = ((day01task1 xs), (day01task2 xs))

day01task1 :: [Int] -> Int
day01task1 xs = length $ filter
        (\(a, b) -> a < b)
        $ zip xs (tail xs)

day01task2 :: [Int] -> Int
day01task2 xs = day01task1 $ sumTries xs

sumTries :: [Int] -> [Int]
sumTries xs = map
    (\(a, b, c) -> a+b+c)
    $ zip3 xs (tail xs) (drop 2 xs)

day02 :: [String] -> (Int, Int)
day02 xs = ((day02task1 xs), (day02task2 xs))

day02task1 :: [String] -> Int
day02task1 s = x * y
    where (x, y) = day02fold1 s

day02task2 :: [String] -> Int
day02task2 s = x * y
    where (_, x, y) = day02fold2 s

day02fold1 :: [String] -> (Int, Int)
day02fold1 s = foldr
        (\(instr, n) vals -> day02move1 instr n vals)
        (0, 0)
        $ (map labeledInt s)

day02move1 :: String -> Int -> (Int, Int) -> (Int, Int)
day02move1 "down" n (x, y) =  (x, y+n)
day02move1 "up" n (x, y) = (x, y-n)
day02move1 "forward" n (x, y) = (x+n, y)
day02move1 _ _ _ = error "no matching movement"

day02fold2 :: [String] -> (Int, Int, Int)
day02fold2 s = foldr
        (\(instr, n) vals -> day02move2 instr n vals)
        (0, 0, 0)
        $ (reverse (map labeledInt s))

day02move2 :: String -> Int -> (Int, Int, Int) -> (Int, Int, Int)
day02move2 "down" n (aim, x, y) =  (aim+n, x, y)
day02move2 "up" n (aim, x, y) = (aim-n, x, y)
day02move2 "forward" n (aim, x, y) = (aim, x+n, y+(aim*n))
day02move2 _ _ _ = error "no matching movement"