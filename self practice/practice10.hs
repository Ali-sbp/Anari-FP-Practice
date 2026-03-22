import Data.Char(isLower,isUpper,digitToInt,isDigit, isSpace, GeneralCategory (LowercaseLetter))
import Control.Applicative(Alternative(..), optional, ZipList(..))



--import Main (satisfy)
--import Lec07 (Parser(runParser))

newtype Parser tok a = Parser {runParser :: [tok] -> Maybe ([tok], a)}

charA :: Parser Char Char 
charA = Parser f where 
    f (x:xs) | x =='A' = Just (xs, x)
    f                _ = Nothing

tst1 = runParser charA "BCSA"

satisfy :: (tok -> Bool) -> Parser tok tok 
satisfy p = Parser f where 
    f (x:xs) | p x = Just (xs ,x)
    f           _  = Nothing 

tst2 = runParser (satisfy isDigit) "1asd"

char :: Eq tok => tok -> Parser tok tok
char c = satisfy (==c)

lower :: Parser Char Char   
lower = satisfy isLower 

instance Functor (Parser tok) where 
    fmap g (Parser u) = Parser f where 
        f xs = case u xs of 
            Nothing -> Nothing
            Just(tok', x) -> Just(tok', g x) 
    
    --fmap g (Parser p) = Parser (fmap (fmap (fmap g) ) p)

digit :: Parser Char Int 
digit = digitToInt <$> (satisfy isDigit)

tst5= runParser digit "x1asd"

charB :: Parser Char Char 
charB = Parser f where 
    f (x:xs) | x == 'B' = Just(xs , x)
    f               _   = Nothing
tstB = runParser charB "Basd"

space :: Parser Char Char 
space = satisfy isSpace

tstspace = runParser space " asd"

charF :: Char -> Parser Char Char 
charF c = satisfy (==c)

testcharF = runParser (charF 'd') "dasdas"

upper :: Parser Char Char
upper = satisfy isUpper

tstupper = runParser upper "Asdsd"

digitParser :: Parser Char Int 
digitParser = digitToInt <$> satisfy isDigit

tstdigitParser = runParser digitParser "21asd"

newtype Parser1 tok a = Parser1 {runParser1 :: [tok] -> Maybe ([tok], a)}

instance Functor (Parser1 tok) where 
    fmap g (Parser1 u) = Parser1 f where 
        f xs = case u xs of 
            Nothing -> Nothing 
            Just(xs', x) -> Just(xs', g x)

satisfy' :: (tok -> Bool) -> Parser1 tok tok
satisfy' p = Parser1 f where 
    f (x : xs) | p x = Just (xs , x)
    f              _  = Nothing

digit' :: Parser1 Char Int 
digit' = digitToInt <$> satisfy' isDigit
tstParser1 = runParser (fmap (*10) digit ) "7asd" 

instance Applicative (Parser tok) where 

    pure :: a -> Parser tok a 
    pure a = Parser (\input -> Just(input, a))
    (<*>) :: Parser tok (a -> b) -> Parser tok a -> Parser tok b 
    Parser u <*> Parser v = Parser f where 
        f xs = case u xs of 
            Nothing -> Nothing 
            Just(tok', g) -> 
                case v tok' of 
                    Nothing -> Nothing
                    Just(tok'', a) -> Just(tok'', g a)
tstpure= runParser (pure 45) "asd"

multipP :: Parser Char Int 
multipP = (*) <$> digit <*> (char '*' *> digit)

tstmult = runParser multipP "13*4"

twoDigits :: Parser Char (Int, Int) 
twoDigits = (,) <$> digit <*> digit 

tstTwod = runParser twoDigits "12"

instance Alternative (Parser tok) where 
    empty :: Parser tok a 
    empty = Parser f where 
        f (x: xs ) = Nothing

    (<|>) :: Parser tok a -> Parser tok a -> Parser tok a 
    Parser u <|> Parser v = Parser f where 
        f xs = case u xs of 
            Nothing -> v xs 
            result -> result 

tstalt1 = runParser (char 'a' <|> char 'b') "fbss"

lowers :: Parser Char String
lowers = (:) <$> lower <*> lowers <|> pure []

tstaltlowers = runParser lowers "asdv"

--digits' :: Parser Char Int 
--digits' = (\x y-> x*10 + y) <$> digit <*> digits' <|> 0 

{-
some :: f a -> f [a]
some v = (:) <$> v <*> many v

many :: f a -> f [a]  :              one or more  : parse v , then zero-or-more
many v = some v <|> pure []          zero or more : try some, or return p[]

optional :: Alternative f => f a -> f (Maybe a)
optional v = Just <$> v <|> pure Nothing : try v , wrap in Just or return Nothing
-}

finalMult :: Parser Char Int 
finalMult = (*) <$> number <*> (char '*' *> number) where 
    number = foldl (\x d -> x * 10 + d)0 <$> some digit 

tstfmult = runParser finalMult "12*231"

s = runParser (optional (char 'a')) "asdas"
--            (Maybe Char)

digits'' :: Parser Char Int 
digits'' = foldl(\x d -> x*10 + d) 0 <$> some digit 

tst'' = runParser digits'' "213a23" 
getVal :: Maybe ([Char], Int) -> Int 
getVal (Just (_, x)) = x 
getVal Nothing = 0  
tstgetval = 2 * getVal tst''
finalPlus :: Parser Char Int
finalPlus = (+) <$> number <*> (char '+' *> number) where
    number = foldl (\x d -> x *10 + d) 0 <$> some digit 

plusormult :: Parser Char Int 
plusormult = finalMult <|> finalPlus

tstfnal = runParser plusormult "12*345"

tstaaa = runParser plusormult "12*345dsf"