import Test.HUnit

import Test2021
import Test2022

main :: IO ()
main = runTestTTAndExit tests

tests :: Test
tests = TestList [
    TestLabel "2021" tests2021,
    TestLabel "2022" tests2022]