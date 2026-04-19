module Main (main) where

import GameEngine

main :: IO ()
main = do
    maze <- loadMaze "maze.txt"
    runGame gameLoop maze
