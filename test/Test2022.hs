{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Test2022 (tests2022) where

import Test.HUnit
import Data.String
import Data.FileEmbed

import Lib2022

tests2022 :: Test
tests2022 = TestList [
    day01test
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