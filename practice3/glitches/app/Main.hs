module Main (main) where

import Purefuncs
import qualified Data.ByteString as B 
import qualified Data.ByteString.Char8 as BC 
import System.Random(randomRIO)
import System.Environment (getArgs)
import Control.Monad(foldM)

main :: IO ()
main = do 
    args <- getArgs
    let fileName = head args
    imageFile <- BC.readFile fileName
    glitched <- randomReplaceByte imageFile
    let glitchedFileName = mconcat ["glitched1_",fileName]
    BC.writeFile glitchedFileName glitched
    putStrLn "glitch 1 (random byte sway) is done!"
    glitched <- randomSortSection imageFile 
    let glitchedFileName = mconcat ["glitched2_", fileName]
    BC.writeFile glitchedFileName glitched 
    putStrLn "glitch 2 (random sort section) is done!"
--combination of glitches as explained in book
    -- glitched1 <- randomReplaceByte imageFile
    -- glitched2 <- randomSortSection glitched1
    -- glitched3 <- randomReplaceByte glitched2
    -- glitched4 <- randomSortSection glitched3
    -- glitched5 <- randomReplaceByte glitched4
    
    --using FoldM from Monad as said in the book instead
    glitched <- foldM (\bytes func -> func bytes) imageFile
        glitchActions
    let glitchedFileName = mconcat ["glitched_3xRep_2xSort",fileName]
    BC.writeFile glitchedFileName glitched
    putStrLn "glitch 3 (3xRep + 2x Sort) is done!"


--impure functions .

--glitch 1 random byte swap and onwards
randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
    let bytesLength = BC.length bytes
    location <- randomRIO (1, bytesLength)
    chV <- randomRIO (0, 255)
    return (replaceByte location chV bytes)

--glitch 2: 
-- 
randomChar :: IO Char
randomChar = do
    randomInt <- randomRIO (0,255)
    return (toEnum randomInt)
randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do 
    let sectionSize = 25 
    let bytesLength = BC.length bytes
    start <- randomRIO (0,bytesLength - sectionSize)
    return (sortSection start sectionSize bytes)

glitchActions :: [BC.ByteString -> IO BC.ByteString]
glitchActions = [randomReplaceByte ,randomSortSection
    ,randomReplaceByte ,randomSortSection
    ,randomReplaceByte]