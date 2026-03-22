module MyParsers.MyParsec () where


import Text.Parsec (Parsec, parse, digit, char, many1,try,ParseError ,(<|>))
import Text.Parsec.String (Parser)  -- Parser = Parsec String () a
import Data.Char (digitToInt)
 

    
digits :: Parser  Int 
digits = foldl(\x d -> x*10 + d) 0  <$> many1(digitToInt <$>  digit )

multP :: Parser Int 
multP = (*) <$> digits <* char '*' <*> digits 

plusP :: Parser Int 
plusP = (+) <$> digits <* char '+' <*> digits 

plusOrMultParsec :: Parser Int 
plusOrMultParsec = try multP <|> plusP

--helper to keep the integrity of main
runParser :: Parser Int -> String -> Either ParseError Int 
runParser p input = parse p "" input 


tst1 = runParser plusOrMultParsec "12*345asd"

{-
ghci> :t parse
parse
  :: Text.Parsec.Prim.Stream s Data.Functor.Identity.Identity t =>
     Parsec s () a
     -> Text.Parsec.Pos.SourceName -> s -> Either ParseError a

     reason why asd is not shown is because parse throws it away! only returns Either Error or result
-}