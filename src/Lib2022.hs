module Lib2022 (
    day01,
    ) where

import Lib

day01 :: String -> (Int, Int)
day01 s = (day01task1 s, day01task2 s)

day01task1 :: String -> Int
day01task1 s = maximum (day01elfs s)

day01task2 :: String -> Int
day01task2 s = sum top3
    where
        top3 = take 3 (reverse (sort (day01elfs s)))

day01elfs :: String -> [Int]
day01elfs s = (map sum vals)
    where
        xs = separateSubstrings (lines s) $ ""
        vals = map (\x -> map atoi x) xs