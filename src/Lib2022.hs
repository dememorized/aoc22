{-# LANGUAGE OverloadedStrings #-}

module Lib2022 (
    day01,
    day02,
    day03,
    day04,
    day05,
    day06,
    day07,
    day07readFiles,
    day07prompt
    ) where

import Lib
import Data.Char (ord)
import qualified Data.Set as Set

day01 :: String -> (Int, Int)
day01 s = (day01task1 s, day01task2 s)

day01task1 :: String -> Int
day01task1 s = maximum $ day01elfs s

day01task2 :: String -> Int
day01task2 s = sum . take 3 . reverse . sort $ day01elfs s

day01elfs :: String -> [Int]
day01elfs s = map sum . map (map atoi) $ separateSubstrings (lines s) ""

day02 :: String -> (Int, Int)
day02 s = (day02task s day02score, day02task s day02strategy)

day02task :: String -> (String -> String -> Int) -> Int
day02task s fn = sum . map (day02processLine fn) . map words $ lines s

day02processLine :: (String -> String -> Int) -> [String] -> Int
day02processLine func (first:second:_) = func first second
day02processLine _ _ = error "invalid input"

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

day03 :: String -> (Int, Int)
day03 s = (day03task1 compartments, day03task2 rucksacks)
    where
        rucksacks = lines s
        compartments = map day03compartments rucksacks

day03compartments :: String -> (String, String)
day03compartments s = (take half s, drop half s)
    where half = div (length s) 2

day03task1 :: [(String, String)] -> Int
day03task1 [] = 0
day03task1 ((a, b):rest) = day03values (day03overlappingChars a b) + day03task1 rest

day03task2 :: [String] -> Int
day03task2 [] = 0
day03task2 (first:second:third:rest) = val + day03task2 rest
    where
        fstSnd = day03overlappingChars first second
        val = day03values (day03overlappingChars fstSnd third)
day03task2 _ = error "expected all groups to be groups of threes"

day03overlappingChars :: String -> String -> String
day03overlappingChars a b = Set.toList $ Set.intersection (Set.fromList a) (Set.fromList b)

day03values :: String -> Int
day03values xs = sum (map day03value xs)

day03value :: Char -> Int
day03value c
    | c >= 'a' && c <= 'z' = ord c - ord 'a' + 1
    | c >= 'A' && c <= 'Z' = ord c - ord 'A' + 27
    | otherwise = error "not an alphabetical character"

day04 :: String -> (Int, Int)
day04 s = (day04task s day04superset, day04task s day04overlap)

type Day04Pair = ((Int, Int), (Int, Int))

day04task :: String -> (Day04Pair -> Bool) -> Int
day04task s filterFn = length . filter filterFn . map day04subranges $ (lines s)

day04superset :: Day04Pair -> Bool
day04superset ((aA, aZ), (bA, bZ)) = (aA <= bA && aZ >= bZ) || (bA <= aA && bZ >= aZ)

day04overlap :: Day04Pair -> Bool
day04overlap ((aA, aZ), (bA, bZ)) = (max aA bA) <= (min aZ bZ)

day04subranges :: String -> Day04Pair
day04subranges s = ((elfs !! 0 !! 0, elfs !! 0 !! 1), (elfs !! 1 !! 0, elfs !! 1 !! 1))
    where elfs = map (map atoi) . map (\elf -> readUntil elf '-') $ readUntil s ','

day05 :: String -> (String, String)
day05 s = (
        day05top . foldl (day05moveInstruction day05moveRepeated) stack $ lines s,
        day05top . foldl (day05moveInstruction day05move) stack $ lines s
    )
    where stack = day05stack . reverse $ lines s

type Day05Stack = [[Char]]

day05stack :: [String] -> Day05Stack
day05stack [] = []
day05stack ((' ' : '1' : line): rest) = day05stack' rest $ take columns $ repeat ""
        where
            -- 4 characters per column, we have already extracted 2 characters, and the last element misses the trailing space
            columns = div ((length line) + 3) 4
day05stack (_: rest) = day05stack rest

day05stack' :: [String] -> Day05Stack -> Day05Stack
day05stack' [] coll = coll
day05stack' (l:rest) coll = day05stack' rest (day05applyLine l coll)

day05applyLine :: String -> Day05Stack -> Day05Stack
day05applyLine l coll = map (\(curr, add) -> add ++ curr) . zip coll $ day05readLine l $ length coll

day05readLine :: String -> Int -> [String]
day05readLine [] len = take len $ repeat ""
day05readLine (' ' : ' ' : ' ' : rest) len = [""] ++ day05readLine' rest (len-1)
day05readLine ('[' : c : ']' : rest) len = [[c]] ++ day05readLine' rest (len-1)
day05readLine _ _ = error "invalid formatting"

day05readLine' :: String -> Int -> [String]
day05readLine' (' ' : rest) len = day05readLine rest len
day05readLine' [] len = day05readLine [] len
day05readLine' _ _ = error "invalid formatting (2)"

day05moveInstruction :: (Day05Stack -> Int -> Int -> Int -> Day05Stack) -> Day05Stack -> String -> Day05Stack
day05moveInstruction fn curr ('m' : 'o' : 'v' : 'e' : ' ': rest) = fn curr (atoi n) ((atoi from)-1) ((atoi to)-1)
    where
        (n, rest1) = span (/= ' ') rest
        (from, rest2) = span (/= ' ') $ drop 6 rest1 -- ' from '
        to = drop 4 rest2 -- ' to '
day05moveInstruction _ curr _ = curr

day05moveRepeated :: Day05Stack -> Int -> Int -> Int -> Day05Stack
day05moveRepeated stack 0 _ _ = stack
day05moveRepeated stack n from to = day05moveRepeated (day05move stack 1 from to) (n-1) from to

day05move :: Day05Stack -> Int -> Int -> Int -> Day05Stack
day05move stack n from to = replaceNth fromReplaced to (moves ++ stack !! to)
    where
        (moves, stays) = splitAt n $ stack !! from
        fromReplaced = replaceNth stack from stays

day05top :: Day05Stack -> String
day05top stack = map (\x -> if length x == 0 then ' ' else head x) stack

day06 :: String -> (Int, Int)
day06 s = (day06uniqueSubsequence s 4 4, day06uniqueSubsequence s 14 14)

day06uniqueSubsequence :: String -> Int -> Int -> Int
day06uniqueSubsequence s n i
    | length(s) < n = -1
    | unique == n = i
    | otherwise = day06uniqueSubsequence (tail s) n (i+1)
    where unique = length . Set.fromList $ take n s

day07 :: String -> (Int, Int)
day07 s = (
        sum . filter (<100000) $ map day07dirSize allDirectories,
        head . sort . filter (>= missingSpace) $ map day07dirSize allDirectories
        )
    where
        session = day07prompt "" (lines s) []
        rootSize = day07dirSize $ day07nodesRecursive session ""
        missingSpace = 30000000 - (70000000 - rootSize)
        allDirectories = map (day07nodesRecursive session) . map (\(File dir _ _) -> dir) $ day07directories session

day07directories :: [[Node]] -> [Node]
day07directories fs = filter (\(File _ filename _) -> filename == "") . concat $ fs

day07nodesRecursive :: [[Node]] -> String -> [Node]
day07nodesRecursive fs cwd = filter (\(File dir _ _) -> cwd == (take (length cwd) dir)) . concat $ fs

day07dirSize :: [Node] -> Int
day07dirSize nodes = sum $ map (\(File _ _ size) -> size) nodes

data Op = ChDir String | DirListing String [Node] deriving (Eq, Ord, Show)
data Node = File String String Int deriving (Eq, Ord, Show)

day07prompt :: String -> [String] -> [[Node]] -> [[Node]]
day07prompt _ [] state = state
day07prompt cwd input state = case cmd of
    DirListing _ nodes -> day07prompt cwd nextInput (nodes : state)
    ChDir newDir -> day07prompt newDir nextInput state
    where
        (cmd, nextInput) = day07prompt' cwd input

day07prompt' :: String -> [String] -> (Op, [String])
day07prompt' cwd (('$': ' ': cmd): output) = case (day07parseCommand cwd cmd) of
    DirListing _ _ -> day07readFiles cwd output
    ChDir newDir -> (ChDir newDir, output)
day07prompt' _ _ = error "expected each line of prompt to start with $"

day07readFiles :: String -> [String] -> (Op, [String])
day07readFiles cwd output = (DirListing cwd nodes, rest)
    where
        (files, rest) = break (\s -> (head s) == '$') output
        nodes = map (day07readFile cwd) . map (words) $ files

day07readFile :: String -> [String] -> Node
day07readFile cwd ("dir": name : []) = File (cwd ++ "/" ++ name) "" 0 
day07readFile cwd xs = File cwd (last xs) (atoi $ head xs)

day07parseCommand :: String -> String -> Op
day07parseCommand cwd cmd
    | take 3 cmd == "cd " = day07chdir cwd $ drop 3 cmd
    | take 2 cmd == "ls" = DirListing cwd []
    | otherwise = error "no such command or directory"

day07chdir :: String -> String -> Op
day07chdir cwd newDir
    | newDir == "/" = ChDir ""
    | newDir == ".." = ChDir $ reverse . dropWhile (== '/') . dropWhile (/= '/') $ reverse cwd
    | otherwise = ChDir $ cwd ++ "/" ++ newDir
