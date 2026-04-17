-- chapter 30
main :: IO () 
main = askForName >>
        getLine >>=
            (\name -> 
                return (nameStatement name)) >>=
                    putStrLn
    
-- getLine :: IO String
-- PutStrLn :: String -> IO ()
--echo :: IO String -> (String -> IO () ) -> IO () 
echo :: IO () 
echo =  getLine >>= putStrLn 

-- Monad m :: m a -> (a -> m b) -> m b

readInt :: IO Int
readInt = read <$> getLine

printDouble :: Int -> IO ()
printDouble n = print (n*2) 

-- IO Int -> (Int -> IO () ) -> IO () 
chain :: IO () 
chain = readInt >>= printDouble

chainVerbose :: IO () 
chainVerbose = putStrLn " we return the double of ur value brah" >> 
                (read <$> getLine) >>= printDouble

askForName :: IO ()
askForName = putStrLn " whats ur name brah?"

-- readStr :: IO String
-- readStr = read <$> getLine

nameStatement :: String -> String 
nameStatement name = "yo " ++ name ++ " !!, Sup??"

-- Quick check 30.4 Turn (+ 2) from type Num a => a -> a to type Num a => a -> IO a using a
-- lambda and return. Use :t in GHCi to double-check that you’re getting the correct type.
-- asd =(\f -> return ((+2) f)) TO ASK

-- Q30.1 To prove that Monad is strictly more powerful than Functor, write a universal ver-
-- sion of <$>, as in the preceding lesson’s exercise, called allFmapM, that defines <$> for all
-- members of the Monad type class. Because it works for all instances of Monad, the only func-
-- tions you can use are the methods required by the Monad type class (and lambda func-
-- tions). To get you started, here’s your type signature:

allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM f ma = ma >>= (\x -> return (f x))

-- Q30.2 To prove that Monad is strictly more powerful than Applicative, write a universal
-- version of <*>, called allApp, that defines <*> for all members of the Monad type class.
-- Because it works for all instances of Monad, the only functions you can use are the meth-
-- ods required by the Monad type class (and lambda functions). To get you started, here’s
-- your type signature:
allApp :: Monad m => m (a -> b) -> m a -> m b
allApp mf ma = mf >>= (\f -> ma >>=
                       \x -> return (f x))
-- Q30.3 Implement a bind function which is the same as (>>=) for Maybe:
-- bind :: Maybe a -> (a -> Maybe b) -> Maybe b
-- This question is much trickier than the last one. Two hints:
-- Try to think exclusively in terms of the type signatures.
-- Use <$> if you want and replace it with your answer to Q29.1
bind :: Maybe a -> (a -> Maybe b) -> Maybe b 
bind ma f = case ma of 
    Nothing -> Nothing 
    Just a -> f a 
        


