module MyParsers.MyParser (runParser,charA, satisfy, char, lower, upper, digit, multiplication, lowers, digits, finalMult,finalPlus,plusOrMult) where

import MyTypes.MyMaybe (MyMaybe(..))


import Data.Char(isLower,isUpper,digitToInt,isDigit)
import Control.Applicative(Alternative(..))



newtype Parser tok a = Parser {runParser :: [tok] -> MyMaybe ([tok], a)}

charA :: Parser Char Char 
charA = Parser f where 
    f (x : xs ) | x =='A' = MyJust(xs , x)
    f               _ = MyNothing

satisfy :: (tok -> Bool) -> Parser tok tok 
satisfy p = Parser f where 
    f ( x : xs )  | p x = MyJust(xs , x)
    f               _   = MyNothing


char :: Char -> Parser Char Char 
char c = satisfy (==c)

lower :: Parser Char Char 
lower = satisfy isLower

upper :: Parser Char Char 
upper = satisfy isUpper

digit :: Parser Char Int 
digit = digitToInt <$> satisfy isDigit

instance Functor (Parser tok) where 
   -- fmap :: (a->b) -> Parser tok a -> Parser tok b 
    fmap g (Parser u) = Parser f where 
        f xs = case u xs of 
            MyNothing -> MyNothing
            MyJust(xs', x) -> MyJust(xs', g x)

instance Applicative (Parser tok) where 
   -- pure :: a -> Parser tok a 
    pure x = Parser (\asd -> MyJust (asd, x))

   -- (<*>) :: Parser tok a -> Parser tok a -> Parser tok a 
    Parser u <*> Parser v = Parser f where 
        f xs = case u xs of 
            MyNothing -> MyNothing 
            MyJust(xs', g) -> case v xs' of 
                MyNothing -> MyNothing
                MyJust (xs'', x) -> MyJust(xs'', g x) 

instance Alternative (Parser tok) where 
   -- empty :: Parser tok a 
    empty = Parser (\_ -> MyNothing)

   -- (<|>) :: Parser tok a -> Parser tok a -> Parser tok a 
    Parser u <|> Parser v = Parser f where 
        f xs = case u xs of 
            MyNothing -> v xs
            result -> result 

multiplication :: Parser Char Int 
multiplication = (*) <$> digit <* char '*' <*> digit

lowers :: Parser Char String 
lowers = ((:) <$> lower <*> lowers) <|> pure [] -- or pure "". same thing in this context

digits :: Parser Char Int 
digits = foldl(\x d -> x*10 + d) 0 <$> some digit


finalMult :: Parser Char Int 
finalMult = (*) <$> digits <* char '*' <*> digits 

finalPlus :: Parser Char Int 
finalPlus = (+) <$> digits <* char '+' <*> digits 

plusOrMult :: Parser Char Int 
plusOrMult = finalMult <|> finalPlus 



