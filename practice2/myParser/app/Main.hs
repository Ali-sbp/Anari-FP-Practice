module Main (main) where

import MyTypes.MyTree
import MyTypes.MyMaybe
import MyTypes.MyEither
import MyParsers.MyParser
import MyParsers.MyParsec (plusOrMultParsec, runParser')
import MyParsers.MyAttoparsec(plusOrMultAttoparsec, runParser'')

main :: IO ()
main = do
    --some random tests 
  -- MyTree
  let t = Node 1 (Leaf 2) (Node 3 (Leaf 4) (Leaf 5))
  putStrLn (show (fmap (*2) t))
  putStrLn (show (foldr (+) 0 t))

  -- MyMaybe
  putStrLn (show (MyJust "Hello" <> MyJust " World"))
  putStrLn (show (MyJust (+3) <*> MyJust 7))

  -- MyEither
  putStrLn (show (fmap (*2) (MyRight 21) :: MyEither String Int))
  putStrLn (show (MyLeft "err1" <> MyLeft "err2" :: MyEither String String))

  --MyParser 
  putStrLn "MyParser:"
  putStrLn $ show (runParser plusOrMult "12*345dsf")
  putStrLn $ show (runParser plusOrMult "12+345dsf")
    
  putStrLn "Parsec:"
  putStrLn $ show (runParser' plusOrMultParsec "12*345dsf")
  putStrLn $ show (runParser' plusOrMultParsec "12+345dsf")
  
  putStrLn "Attoparsec:"
  putStrLn $ show (runParser'' plusOrMultAttoparsec "12*345dsf")
  putStrLn $ show (runParser'' plusOrMultAttoparsec "12+345dsf")
    