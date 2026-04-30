import Test.QuickCheck
import Primes

import Data.Maybe

main :: IO ()
-- main = putStrLn "Test suite not yet implemented"
-- main = quickCheck prop_validPrimesOnly

main = do 
    putStrLn "Running QuickCheck...(validPrimesOnly)"
    quickCheck prop_validPrimesOnly
    putStrLn "Running QuickCheck...(prop_primesArePrime)"
    quickCheckWith stdArgs {maxSuccess = 1000} prop_primesArePrime
    putStrLn "Running QuickCheck...(nonPrimesAreComposite)"
    quickCheckWith stdArgs {maxSuccess = 1000} prop_nonPrimesAreComposite
    putStrLn "Running QuickCheck...(prop_factorsMakeOriginal)"
    quickCheck prop_factorsMakeOriginal
    putStrLn "Running QuickCheck...(prop_allFactorsPrime)"
    quickCheck prop_allFactorsPrime
    putStrLn "done"

----- 
-- all the tests in the book uses things like val >= length primes which are wrong! 
--     i am changing it into manual values 
-- prop_validPrimesOnly val = if val < 0 || val >= (head . reverse $ primes)
--                             then result == Nothing
--                             else isJust result
--     where result = isPrime val

prop_validPrimesOnly :: Int -> Bool
prop_validPrimesOnly val = if val < 2 || val > (head . reverse $ primes)
                           then isNothing result
                           else isJust result
    where result = isPrime val

-- generating a list of all the numbers less than your prime, starting at 2. Then
-- you’ll filter this list to see if any of the values evenly divide your input number.

prop_primesArePrime :: Int -> Bool
prop_primesArePrime val = if result == Just True then length divisors == 0 else True
    where result = isPrime val
          divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]


-- if your function returns Just False, the input number 
-- has at least one number
-- that’s less than it and evenly divides it.

prop_nonPrimesAreComposite :: Int -> Bool
prop_nonPrimesAreComposite val = if result == Just False then length divisors > 0 else True
    where result = isPrime val
          divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

prop_factorsMakeOriginal :: Int -> Bool
prop_factorsMakeOriginal val = if result == Nothing
                               then True
                               else product (fromJust result) == val
    where result = primeFactors val

prop_allFactorsPrime :: Int -> Bool
prop_allFactorsPrime val = if result == Nothing
                           then True
                           else all (== Just True) resultsPrime
    where result = primeFactors val
          resultsPrime = map isPrime (fromJust result)
