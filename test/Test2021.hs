{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Test2021 (tests2021) where

import Test.HUnit
import Data.String
import Data.FileEmbed

import Lib
import Lib2021

tests2021 :: Test
tests2021 = TestList [
    day01test,
    day02test
    ]

day01sample :: String
day01sample = "199\n200\n208\n210\n200\n207\n240\n269\n260\n263"

day01input :: IsString a => a
day01input = $(embedStringFile "test/2021/01")

day01test :: Test
day01test = TestList [
    TestLabel "Day 1 Sample" $ TestCase $ assertEqual "Test" (7, 5) (day01 (linesOfInts day01sample)),
    TestLabel "Day 1 Input" $ TestCase $ assertEqual "Test" (1832, 1858) (day01 (linesOfInts day01input))
    ]

day02sample :: String
day02sample = "forward 5\ndown 5\nforward 8\nup 3\ndown 8\nforward 2"

day02input :: IsString a => a
day02input = $(embedStringFile "test/2021/02")

day02test :: Test
day02test = TestList [
    TestLabel "Day 1 Sample" $ TestCase $ assertEqual "Test" (150, 900) (day02 (lines day02sample)),
    TestLabel "Day 1 Input" $ TestCase $ assertEqual "Test" (1813801, 1960569556) (day02 (lines day02input))
    ]