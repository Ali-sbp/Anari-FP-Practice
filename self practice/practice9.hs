import Data.Char(isLower,isUpper,digitToInt,isDigit)
import Control.Applicative(Alternative(..), optional, ZipList(..))




newtype Parser tok a = Parser {runParser :: [tok] -> Maybe ([tok], a) }

charA :: Parser Char Char 
charA = Parser f where
    f (x : xs) | x == 'A' = Just(xs ,x)
    f _                   = Nothing 

tst1 = runParser charA "ABC"





satisfy :: (tok -> Bool) -> Parser tok tok
satisfy p = Parser f where
    f (x:xs) | p x = Just (xs, x)
    f _             = Nothing

tst2 = runParser (satisfy (=='A')) "Asd"

char :: Char -> Parser Char Char
char c = satisfy (== c)
lower :: Parser Char Char 
lower = Parser f where
    f (x : xs) | isLower x = Just(xs ,x)
    f _                   = Nothing 

tst3 = runParser lower "asd"

--isDigit
digit :: Parser Char Char 
digit = Parser f where
    f (x : xs) | isDigit x = Just(xs ,x)
    f _                   = Nothing 

--isUpper

instance Functor (Parser tok) where 
    fmap :: (a -> b) -> Parser tok a -> Parser tok b
    --fmap g (Parser u) = Parser f where 
        --f :: [tok] -> Maybe ([tok], a)
     --   f s = case u s of 
       --     Nothing -> Nothing
          --  Just (s', a) -> Just (s', g a)
    
    fmap g (Parser p) = Parser ((fmap . fmap . fmap)g p) --g . runParser ?! ***** 
    --fmap g = Parser $ ( fmap . fmap . fmap ) g . runParser
    --[tok] , Maybe , (,) [tok]
    --fmap g (Parser p) = Parser (fmap (fmap (fmap g)) p)

instance Applicative (Parser tok) where 
    pure :: a -> Parser tok a
    pure x = Parser (\s -> Just (s, x))


    (<*>) :: Parser tok (a -> b) -> Parser tok a -> Parser tok b
    Parser u <*> Parser v = Parser f where
        f xs = case u xs of
            Nothing      -> Nothing
            Just (xs', g) -> case v xs' of
                Nothing       -> Nothing
                Just (xs'', a) -> Just (xs'', g a)

--multiplication :: Parser Char Int 
--multiplication = (*) <$> digit <*> (char '*' *> digit)

-- what exactly does mappend do? 
tst4 = (Just "asd" <|> (Just "fgh"))

instance Alternative (Parser tok) where
    empty :: Parser tok a
    empty = Parser (\_ -> Nothing)
    -- Always fails, ignoring input

    (<|>) :: Parser tok a -> Parser tok a -> Parser tok a
    Parser u <|> Parser v = Parser f where
        f xs = case u xs of
            Nothing -> v xs       -- first failed, try second on SAME input
            result  -> result    -- first succeeded, use its result


lowers :: Parser Char String
lowers = ((:) <$> lower <*> lowers)  <|> pure "" -- if we remove <|> pure "" we will get Nothing in this case : runParser lowers "abdef"


--UNDERSTAND some and many *********************
-- line 442 - 465 


--Используем все методы для получения парсера для выражения типа "12*345"

digits :: Parser Char Int
digits = foldl (\acc c -> acc * 10 + digitToInt c) 0 <$> some digit

finalMult :: Parser Char Int
finalMult = (*) <$> digits <*> (char '*' *> digits)
--finalMult = (*) <$> digits <* char '*' <*> digits
{-
runParser finalMult "12*345"
runParser finalMult "12*345dsf"


Что если я хочу не умножать, а складывать?
-}

finalPlus :: Parser Char Int
finalPlus = (+) <$> digits <* char '*' <*> digits --isnt working?!

plusOrMult :: Parser Char Int 
plusOrMult = finalMult <|> finalPlus

--

--

parIF :: [Parser Char Char]
parIF = [char 'i', char 'f']

parTHEN :: [Parser Char Char]
parTHEN = [char 't', char 'h', char 'e', char 'n']

parELSE :: [Parser Char Char]
parELSE = [char 'e', char 'l', char 's', char 'e']

-- runParser sequenceParIF "if123asd"

--part7 and onwards , didnt understand shit :(

-- Traversable SUPER IMPORTANT, understand what happens in there,
--sequenceA , traverse :: ... and so on 

--traversable for Maybe, and FUnctor for Maybe compare 
--instance Traversable [] where 
    -- ... 
--laws of Traversable (1)identity (2)composition (3)naturality inherit from Functor laws ???

--newtype Identity a = Identity { runIdentity :: a } deriving (Show) -- Id :: a -> Id a

-- Alt(..) , Ap(..),Sum(..)

-- 750 - end @Lec_07.hs 

--src 
--    -MyTypes
--    -MyParser/c ?
--          - ...
--          - ...

-- DONT CHANGE
{-
putStrLn "MyParser:"
    putStrLn $ show (runParser plusOrMult "12*345dsf")
    putStrLn $ show (runParser plusOrMult "12+345dsf")
   
-}

{-
putStrLn "Parsec:"
    putStrLn $ show (runParser plusOrMultParsec "12*345dsf")
    putStrLn $ show (runParser plusOrMultParsec "12+345dsf")
    putStrLn "Attoparsec:"
    putStrLn $ show (runParser plusOrMultAttoparsec "12*345dsf")
    putStrLn $ show (runParser plusOrMultAttoparsec "12+345dsf")
can change regarding your stuff
-}