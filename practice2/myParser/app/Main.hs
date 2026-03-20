module Main (main) where

import MyTypes.MyTree
import MyTypes.MyMaybe
import MyTypes.MyEither


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