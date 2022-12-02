{-# LANGUAGE OverloadedStrings #-}

module Lib2022 (
    day01,
    day02
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

day02 :: String -> (Int, Int)
day02 s = (day02task1 s, day02task2 s)

day02task1 :: String -> Int
day02task1 s = sum scores
    where
        raw = map words (lines s)
        scores = map (day02processLine day02score) raw

day02task2 :: String -> Int
day02task2 s = sum scores
    where
        raw = map words (lines s)
        scores = map (day02processLine day02strategy) raw

day02processLine :: (String -> String -> Int) -> [String] -> Int
day02processLine func wl = func (head wl) (last wl)

day02score :: String -> String -> Int
day02score opp self = (day02selectionScore self) + (day02score' opp self)

day02score' :: String -> String -> Int
day02score' opp self
    | self == win = 6
    | self == draw = 3
    | otherwise = 0
    where
        (_, draw, win) = day02trie opp

day02trie :: String -> (String, String, String)
day02trie "A" = ("Z", "X", "Y")
day02trie "B" = ("X", "Y", "Z")
day02trie "C" = ("Y", "Z", "X")
day02trie _ = error "invalid rock paper scissor instructions"

day02selectionScore :: String -> Int
day02selectionScore "X" = 1
day02selectionScore "Y" = 2
day02selectionScore "Z" = 3
day02selectionScore _ = error "invalid rock paper scissor instructions"

day02strategy :: String -> String -> Int
day02strategy opp strat
    | strat == "Z" = day02score opp win
    | strat == "Y" = day02score opp draw
    | otherwise = day02score opp lose
    where (lose, draw, win) = day02trie opp