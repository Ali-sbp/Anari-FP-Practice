module Main (main) where
import qualified Control.Monad.RWS as R 
import qualified Data.Map.Strict as M
import qualified Data.Set as S 
import MazeSolver
import qualified Control.Monad as Control

maze1 :: Maze 
maze1 = M.fromList
    [ ("start",  ["roomA", "roomB", "roomC"])
    , ("roomA",  ["roomA1"])
    , ("roomB",  ["roomB1"])
    , ("roomC",  ["roomC1"])
    , ("roomD",  [])
    , ("roomB1",  [])
    , ("roomC1",  ["finish"])
    , ("roomA1",  ["roomA2"])
    , ("roomA2",  ["roomA3"])
    , ("roomA3",  ["roomA4"])
    , ("roomA4",  [])
    , ("finish",  [])
    ]

main :: IO ()
main = 
    let (path, _, logs) = R.runRWS (dfs "start") maze1 S.empty
    in mapM_ putStrLn logs >>
       putStrLn ("path: " ++ show path)

-- mtl isnt needed, can do with trans only , RWS there is in Control.Monad.Trans.RWS 