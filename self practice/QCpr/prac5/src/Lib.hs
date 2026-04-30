module Lib
    ( addMod
    , reverseWords
    ) where

import Test.QuickCheck 

addMod :: Int -> Int -> Int -> Int
addMod x y m
    | m <= 0 = error "modulus must be positive"
    | otherwise = (x + y) `mod` m


-- prop1 : the definition. Note : using Positive Int here 
--                               to match the function definition guard 

-- prop1 :: Int -> Int -> Positive Int -> Bool
-- prop1 x y (Positive m) =
--     addMod x y m === (x + y) `mod` m
-- quickCheck (verbose prop1)
-- ghci> quickCheck prop1 
-- +++ OK, passed 100 tests.
-- ghci> 

-- prop2 : identity 
-- prop2 :: Int -> Positive Int -> Bool
-- prop2 x (Positive m) =
--     addMod x 0 m == x `mod` m

-- ghci> quickCheck prop2 
-- +++ OK, passed 100 tests.
-- ghci> 

-- prop3 : commutativnost 
-- prop3 :: Int -> Int -> Positive Int -> Bool
-- prop3 x y (Positive m) =
--     addMod x y m == addMod y x m

-- ghci> quickCheck prop3
-- +++ OK, passed 100 tests.
-- ghci> 

------- reverseWords -------------


reverseWords :: String -> String
reverseWords = unwords . reverse . words

-- prop4 : empty string 
-- prop4 = reverseWords "" == ""

-- ghci> quickCheck prop4 
-- +++ OK, passed 100 tests.
-- ghci> 

--prop5 : one word 

-- for this I searched a bit and came across listOf1 :: Gen a -> Gen [a] which generates from given values
-- and after checking the stuff that quickCheck generated (like " a" and "\ndsa" and the other garbage)
-- i limited the generated data this way in order not to discard 1 Milion tests samples 
-- ghci> quickCheck (withMaxSuccess 10000 prop5') 
-- +++ OK, passed 10000 tests; 14586 discarded.
-- ghci> 
-- so instead of discarding all of this, I am limiting the quickCheck data instead.
-- and overall i dont like how trivial this guard logic is : (&& s == unwords (words s))
-- its legit the same backwards logic of the isPalindrome case from the book where 
-- we use is isPunctuation in both the wrapper and the test itself ... 
-- prop5' :: String -> Property
-- prop5' s =
--     length (words s) == 1 && s == unwords (words s) ==> reverseWords s == s

-- prop5 :: String -> Property 
-- prop5 =
--     forAll (listOf1 (elements ['a'..'z'])) $ \word ->
--         reverseWords word == word


-- prop6 changing order in multi word 
-- checking for the reverse order in the list (if i understood the task correctly)
-- using words to limit the garbage generated here. 
-- ghci> :t words
-- words :: String -> [String]

-- Prop6 :: String -> Property 
-- prop6 s =
--     let ws = words s
--     in length ws > 1 ==> words (reverseWords s) == reverse ws

-- ghci> quickCheck (withMaxSuccess 10000 prop6) 
-- +++ OK, passed 10000 tests; 42401 discarded.
-- ghci> 

-- equivalant but in String -> Bool 
-- prop6 :: String -> Bool
-- prop6 s =
--     let ws = words s
--     in length ws <= 1 || words (reverseWords s) == reverse ws


 
-- prop7 : double application returns the original string
-- for the current implementation this holds for normalized strings
-- with single spaces between words and no leading/trailing whitespace

-- prop7 :: String -> Property
-- prop7 s =
--     s == unwords (words s) ==> reverseWords (reverseWords s) == s

-- ghci> quickCheck (withMaxSuccess 10000 prop7) 
-- +++ OK, passed 10000 tests; 9853 discarded.
-- ghci> 

--equivalant but in String -> Bool 
-- prop7 :: String -> Bool
-- prop7 s =
--     s /= unwords (words s) || reverseWords (reverseWords s) == s
