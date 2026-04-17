--pure 5 :: [Int]
import Data.Char(digitToInt)
--tst1 = pure (^2) <*>  Just (digitToInt) <*> Just '2'

f1 :: Int -> Maybe String 
f1 v |v<= 5 = Just "its a num" 
     |otherwise = Nothing 

f2 :: String -> Maybe Bool 
f2 s 
    | s == "its a num" = Just True 
    | otherwise = Nothing 

 
islessthan5 v = pure f2 <*> f1 v 


x2 :: Int -> Maybe Int 
x2 a = Just (a ^2 )
_ = Nothing 

xplus1 :: Int -> Maybe Int 
xplus1 a = Just (a +1)
_ = Nothing

pow2thenP1 a = pure xplus1 <*> x2 a 