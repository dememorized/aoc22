{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Test2022 (tests2022) where

import Test.HUnit
import Data.String
import Data.FileEmbed

import Lib2022

tests2022 :: Test
tests2022 = TestList [
    day01test,
    day02test,
    day03test,
    day04test,
    day05test,
    day06test,
    day07test
    ]

day01sample :: String
day01sample = "1000\n2000\n3000\n\n4000\n\n5000\n6000\n\n7000\n8000\n9000\n\n10000"

day01input :: IsString a => a
day01input = $(embedStringFile "test/2022/01")

day01test :: Test
day01test = TestList [
    TestLabel "Day 1 Sample" $ TestCase $ assertEqual "Test" (24000, 45000) (day01 day01sample),
    TestLabel "Day 1 Input" $ TestCase $ assertEqual "Test" (72017, 212520) (day01 day01input)
    ]

day02sample :: String
day02sample = "A Y\nB X\nC Z"

day02input :: IsString a => a
day02input = $(embedStringFile "test/2022/02")

day02test :: Test
day02test = TestList [
    TestLabel "Day 2 Sample" $ TestCase $ assertEqual "Test" (15, 12) (day02 day02sample),
    TestLabel "Day 2 Input" $ TestCase $ assertEqual "Test" (13565, 12424) (day02 day02input)
    ]

day03sample :: String
day03sample = "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw"

day03input :: IsString a => a
day03input = $(embedStringFile "test/2022/03")

day03test :: Test
day03test = TestList [
    TestLabel "Day 3 Sample" $ TestCase $ assertEqual "Test" (157, 70) (day03 day03sample),
    TestLabel "Day 3 Input" $ TestCase $ assertEqual "Test" (7793, 2499) (day03 day03input)
    ]

day04sample :: String
day04sample = "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"

day04input :: IsString a => a
day04input = $(embedStringFile "test/2022/04")

day04test :: Test
day04test = TestList [
    TestLabel "Day 4 Sample" $ TestCase $ assertEqual "Test" (2, 4) (day04 day04sample),
    TestLabel "Day 4 Input" $ TestCase $ assertEqual "Test" (444, 801) (day04 day04input)
    ]

day05test :: Test
day05test = TestList [
    TestLabel "Day 5 Sample" $ TestCase $ assertEqual "Test" ("CMZ", "MCD") (day05 sample),
    TestLabel "Day 5 Input" $ TestCase $ assertEqual "Test" ("VRWBSFZWM", "RBTWJWMCF") (day05 input)
    ]
    where
        sample = "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 \n\nmove 1 from 2 to 1\nmove 3 from 1 to 3\nmove 2 from 2 to 1\nmove 1 from 1 to 2"
        input = $(embedStringFile "test/2022/05")

day06test :: Test
day06test = TestList $ [
    TestLabel "Day 6 Input" $ TestCase $ assertEqual "Test" (1300,3986) (day06 input)
    ] ++ (map
        (\(expected, s) -> TestLabel ("Day 6 Sample: " ++ s) $ TestCase $ assertEqual "Test" expected (day06 s))
        samples)
    where
        samples = [
                ((-1, -1), "abcab"),
                ((7, 19), "mjqjpqmgbljsphdztnvjfqwrcgsmlb"),
                ((5, 23), "bvwbjplbgvbhsrlpgdmjqwftvncz"),
                ((6, 23), "nppdvjthqldpwncqszvftbrmjlhg"),
                ((10, 29), "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"),
                ((11, 26), "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")
            ]
        input = $(embedStringFile "test/2022/06")

day07test :: Test
day07test = TestList $ map
        (\(expected, filename, s) -> TestLabel ("Day 7 " ++ filename) $ TestCase $ assertEqual "Test" expected (day07 s))
        tests
    where
        tests = [
                ((95437, 24933642), "07.sample", $(embedStringFile "test/2022/07.sample")),
                ((1648397, 1815525), "07", $(embedStringFile "test/2022/07"))
            ]