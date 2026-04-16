module MazeSolver where 

import qualified Control.Monad.RWS as R 
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- using Map.Map String [String] O(log n) instead of just a [(Strung, String)] O(n)
-- but the idea for a maze is (String, [String]) : (current room, [accessible rooms])

type Maze = M.Map String [String]


-- using set here for state RWS r w s a , s :: Set.Set String, for the state.

type Solver a = R.RWS Maze [String] (S.Set String) a 
--just a sample maze for testing
sampleMaze :: Maze
sampleMaze = M.fromList
    [ ("start",  ["roomA", "roomB"])
    , ("roomA",  ["roomC"])
    , ("roomB",  ["roomC", "roomD"])
    , ("roomC",  ["finish"])
    , ("roomD",  [])
    , ("finish",  [])
    ]

-- testing with : R.runRWS :: RWS r w s a -> r -> s -> (a, s, w)

getNeighbors :: String -> Solver [String]
getNeighbors room = 
    R.ask >>= \maze -> return (M.findWithDefault [] room maze)

visitRoom :: String -> Solver ()
visitRoom room = 
    R.get >>= \visited -> 
        R.put (S.insert room visited) >> -- using this for put so we throw away the  return () part 
            R.tell ["Visiting: " ++ room]

-- helper to check if room is visited (to avoid loops in DFS)
isVisited :: String -> Solver Bool 
isVisited room = 
    R.get >>= \visited ->
        return (S.member room visited)

-- multiple rooms in state
-- R.runRWS (isVisited "roomB") sampleMaze (S.fromList ["roomA"])


-- going with two mutually dependant functions like some and many: 
-- tryNeighbors tries each neighbor using dfs 
-- dfs calls tryNeighbors 
tryNeighbors :: [String] -> Solver [String]
tryNeighbors [] = return []
tryNeighbors (n:ns) = 
    dfs n >>= \path -> 
        if null path
        then tryNeighbors ns 
        else return path 


--very simple dfs algorithm using calling tryNeighbors
-- the idea is very close to some and many

dfs :: String -> Solver [String]
dfs room = 
    if room == "finish"
    then R.tell ["Reached: finish"] >> return ["finish"]
    else 
        isVisited room >>= \seen ->
            if seen 
            then return []
            else 
                visitRoom room >>
                getNeighbors room >>= \neighbors ->
                    tryNeighbors neighbors >>= \path ->
                        if null path 
                        then return []
                        else return (room : path)

-- visitRoom room >>               -- mark visited + log
-- getNeighbors room >>= \neighbors ->   -- get connected rooms
-- tryNeighbors neighbors >>= \path ->   -- try each one
-- if null path then return []           -- dead end
-- else return (room : path)             -- prepend current room to path
