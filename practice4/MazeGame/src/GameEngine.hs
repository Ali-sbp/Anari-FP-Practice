module GameEngine where 

import MyTransformerStack 
import Data.Maybe(fromMaybe)
import qualified Data.Map.Strict as M 

type RoomName = String 
type Maze = M.Map String [String] 

-- the transformer stack , wrote it cool xixi xaxa 

type Game a = MyStateT RoomName
                (WriterT [String]
                    (ReaderT Maze IO)) a

runGame :: Game () -> Maze -> IO ()
runGame game maze = do
    ((_, _), movelog) <- runReaderT (runWriterT (runMyStateT game "start")) maze
    putStrLn "\n *** Game Over, Your path: ***"
    mapM_ putStrLn movelog
    --writing to logs.txt 
    saveLogs movelog



sampleMaze :: Maze
sampleMaze = M.fromList
    [ ("start",   ["hallway"])
    , ("hallway", ["start", "library", "garden"])
    , ("library", ["hallway", "finish"])
    , ("garden",  ["hallway"])
    , ("finish",  [])
    ]

getNeighbors :: RoomName -> Game [RoomName]
getNeighbors room = do 
    maze <- lift $ lift ask 
    return (M.findWithDefault [] room maze)

-- writing this so that I dont have to write life 100 times 
io :: IO a -> Game a
io = lift . lift . lift


--using elem : elem :: (Foldable t, Eq a) => a -> t a -> Bool
-- helper to see possible moves from ~> to 
canMoveTo :: RoomName -> RoomName -> Game Bool
canMoveTo from to = do
    neighbors <- getNeighbors from
    return (to `elem` neighbors)

-- one game turn 

gameTurn :: Game Bool -- true ~> keep playing, false ~> stop 
gameTurn = do 
    room <- get 
    neighbors <- getNeighbors room 
    io $ putStrLn $ "\nYou are in: " ++ room 
    io $ putStrLn $ "Neighbors: " ++ show neighbors 
    io $ putStr "Move to (or 'quit'): "
    cmd <- io getLine 
    case cmd of 
        "quit" -> return False 
        _       -> do 
            valid <- canMoveTo room cmd 
            if not valid 
                then do 
                    io $ putStrLn "Invalid move!"
                    return True 
                else do 
                    put cmd 
                    lift $ tell ["moved: " ++ room ++ " -> " ++ cmd ]
                    -- 1 lift for tell (WriterT)
                    return (cmd /= "finish")


gameLoop :: Game () 
gameLoop = do 
    continue <- gameTurn 
    if continue then gameLoop else return () 


-- each line of the file : "roomName neighbor1 neighbor2 ..."
-- for example  "hallway start library garden"
parseMaze :: String -> Maze
parseMaze = M.fromList . map parseLine . lines
  where
    parseLine line = case words line of
        (room:neighbors) -> (room, neighbors)
        []               -> ("",   [])


loadMaze :: FilePath -> IO Maze
loadMaze path = parseMaze <$> readFile path

--adding extra logger to save to file for practice 

saveLogs :: [String] -> IO ()
saveLogs movelog = do
    appendFile "logs.txt" "\n=== New Game ===\n"
    appendFile "logs.txt" (unlines movelog)


