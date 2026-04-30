module Main (main) where

import Lib
import Test.QuickCheck

prop_addModMatchesModuloAddition :: Int -> Int -> Positive Int -> Bool
prop_addModMatchesModuloAddition x y (Positive m) =
    addMod x y m == (x + y) `mod` m

prop_addModHasNeutralElement :: Int -> Positive Int -> Bool
prop_addModHasNeutralElement x (Positive m) =
    addMod x 0 m == x `mod` m

prop_addModIsCommutative :: Int -> Int -> Positive Int -> Bool
prop_addModIsCommutative x y (Positive m) =
    addMod x y m == addMod y x m

prop_reverseWordsEmptyString :: Bool
prop_reverseWordsEmptyString =
    reverseWords "" == ""

prop_reverseWordsSingleWord :: String -> Bool
prop_reverseWordsSingleWord s =
    let ws = words s
    in length ws /= 1 || s /= unwords ws || reverseWords s == s

prop_reverseWordsChangesWordOrder :: String -> Bool
prop_reverseWordsChangesWordOrder s =
    let ws = words s
    in length ws <= 1 || words (reverseWords s) == reverse ws

prop_reverseWordsTwiceReturnsOriginal :: String -> Bool
prop_reverseWordsTwiceReturnsOriginal s =
    s /= unwords (words s) || reverseWords (reverseWords s) == s

main :: IO ()
main = do
    putStrLn "Testing addMod..."
    putStrLn "Checking modular addition matches the mathematical definition..."
    quickCheckWith stdArgs {maxSuccess = 1000} prop_addModMatchesModuloAddition
    putStrLn "Checking 0 is the neutral element for modular addition..."
    quickCheck prop_addModHasNeutralElement
    putStrLn "Checking modular addition is commutative..."
    quickCheck prop_addModIsCommutative
    putStrLn "Testing reverseWords..."
    putStrLn "Checking reversing an empty string returns an empty string..."
    quickCheck prop_reverseWordsEmptyString
    putStrLn "Checking reversing a single word returns the same word..."
    quickCheck prop_reverseWordsSingleWord
    putStrLn "Checking reversing several words changes their order..."
    quickCheck prop_reverseWordsChangesWordOrder
    putStrLn "Checking reversing words twice returns the original normalized string..."
    quickCheck prop_reverseWordsTwiceReturnsOriginal
