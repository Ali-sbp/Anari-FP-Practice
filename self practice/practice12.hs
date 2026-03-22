import Data.Attoparsec.Text (Parser, parseOnly, digit, char, many1)
--import Data.Text.Internal
import Control.Applicative ((<|>))
import Data.Char(digitToInt)
import Data.Text (pack)  -- to convert String -> Text





digitsAtto :: Parser Int 
digitsAtto = foldl(\x d -> x*10 + d) 0 <$> many1 (digitToInt <$> digit)

multAtto :: Parser Int 
multAtto = (*) <$> digitsAtto <* char '*' <*> digitsAtto

plusAtto :: Parser Int 
plusAtto = (+) <$> digitsAtto <* char '+' <*> digitsAtto

plusOrMultAttoparsec :: Parser Int 
plusOrMultAttoparsec = plusAtto <|> multAtto

tst1= parseOnly plusOrMultAttoparsec (pack "12*345")

--
{-
helper to keep integrity of main
note how I pack the string for parseOnly, am I smart? lol 
-}
runParser :: Parser Int -> String -> Either String Int 
runParser p input = parseOnly p (pack input) 
{-
ghci> :t parseOnly 
parseOnly :: Parser a -> Data.Text.Internal.Text -> Either String a
just like parsec just returns the result and throws away the rest
-}
tst2 = runParser plusOrMultAttoparsec "12*345asd"