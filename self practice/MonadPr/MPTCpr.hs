{-# LANGUAGE MultiParamTypeClasses #-}
--{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module MPTC where

import Control.Monad.Fail ( MonadFail(..) )
import Control.Monad.Reader
import Control.Monad.State

data Vector = Vector Int Int deriving(Eq, Show)
data Matrix = Matrix Vector Vector deriving (Eq, Show)

class Mult a b c where 
    (***) :: a-> b -> c 

incCounter :: MonadState Int m => m ()
incCounter = do 
    count <- get 
    put (count + 1)

readerStatePr :: ReaderT String (StateT Int IO) () 
readerStatePr = do 
    env <- ask 
    modify (+1) 
    liftIO $ putStrLn $ "Env: " ++ env 


-- do {result <- runStateT (runReaderT readerStatePr "test") 0; print result; return ()}