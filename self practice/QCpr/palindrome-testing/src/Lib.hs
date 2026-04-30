module Lib
    ( isPalindrome
    , preprocess, someFunc 
    ) where
import Data.Char (isPunctuation)

import Data.Text as T 


someFunc :: IO ()
someFunc = putStrLn "alalalla"

-- preprocess :: String -> String
-- -- preprocess text = filter (not . (`elem` ['!','.',']',':'])) text
-- preprocess text = filter (not . isPunctuation) text


-- isPalindrome :: String -> Bool
-- isPalindrome text = cleanText == reverse cleanText
--                         where cleanText = preprocess text

preprocess :: T.Text -> T.Text
preprocess text = T.filter (not . isPunctuation) text

isPalindrome :: T.Text -> Bool
isPalindrome text = cleanText == T.reverse cleanText
    where cleanText = preprocess text