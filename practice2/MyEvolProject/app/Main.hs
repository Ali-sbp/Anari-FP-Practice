module Main where

import Data.List (sort)
import MyEvolModule (MyEvolution(..), MyEvolution' (..))

main :: IO ()
main = putStrLn $ show $ sort ([Humans, LUCA, Trilobite, Dimetrodon, Cyanobacteria, Purgatorius, Ichthyostega, Australopithecine, Morganucodon, Archaeopteryx] :: [MyEvolution])
--main = putStrLn $ show $ sort ([LUCA .. Humans] :: [MyEvolution])
--main = putStrLn $ show $ sort ([minBound..maxBound] :: [MyEvolution])
--main = putStrLn $ show $ sort ([...] :: [MyEvolution])
--main = putStrLn $ show $ sort ([LUCA' .. Humans'] :: [MyEvolution']) ->> added to demonstrate the difference in Show, cause no manual description.
--confusion with how to execute the code, did some variants on my own , all works. 