{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( atoi,
      linesOfInts,
      labeledInt,
      separateSubstrings,
      sort
    ) where

atoi :: String -> Int
atoi s = read s ::Int

linesOfInts :: String -> [Int]
linesOfInts s = map atoi $ lines s

labeledInt :: String -> (String, Int)
labeledInt s =
    (label, atoi val)
    where
        ws = words s
        label = head ws
        val = head (tail ws)

separateSubstrings :: [String] -> String -> [[String]]
separateSubstrings [] _ = []
separateSubstrings xs eq = x : separateSubstrings rest eq
    where
        (x, rest) = nextSubstring xs eq []

nextSubstring :: [String] -> String -> [String] -> ([String], [String])
nextSubstring [] _ coll = (reverse coll, [])
nextSubstring xs eq coll =
    if x == eq
        then (reverse coll, rest)
        else nextSubstring rest eq (x : coll)
    where
        x = head xs
        rest = tail xs

sort :: Ord sortable => [sortable] -> [sortable]
sort [] = []
sort xs = (sort lesser) ++ eq ++ (sort greater)
    where
        x = head xs
        lesser  = filter (< x) xs
        greater = filter (> x) xs
        eq = filter (== x) xs