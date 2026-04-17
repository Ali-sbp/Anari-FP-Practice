{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-}

module TrMon where

import Control.Monad.Writer
import Control.Monad.Reader
-- import Control.Monad.Trans.State -- transformers
import Control.Monad.State -- mtl
import Control.Monad.Identity ( Identity(..) )
import Control.Monad (msum, mfilter, guard, MonadPlus(..), ap, liftM, MonadFail, Functor)
import Control.Applicative (Alternative(empty, (<|>)), Applicative)
import Data.Monoid (First(..), Sum(..), Product(..))
import Data.Maybe (fromMaybe)
import Data.Functor.Identity (Identity(runIdentity))


-- import Control.Monad.Trans.Reader

-- import Control.Monad.IO.Class (liftIO)

-- type App a = ReaderT Config IO a

-- example :: App ()
-- example = do
--     env <- ask
--     let toPath = configToPath env
--         fromPath = configFromPath env
--     liftIO $ putStrLn (fromPath <> " -> " <> toPath)

-- main :: IO ()
-- main = do
--     let cfg = Config "src" "dst" Nothing Nothing Nothing
--     runReaderT example cfg

type Vegetable = String
type Price     = Double
type Qty       = Double
type Cost      = Double
type PriceList = [(Vegetable, Price)]

prices :: PriceList
prices = [("Potato",13),("Tomato",55),("Apple",48)]


addVegetable' :: Vegetable -> Qty -> Writer (Sum Cost) (Vegetable, Price)
addVegetable' veg qty = do
    let pr = fromMaybe 0 $ lookup veg prices   -- ← prices is HARDCODED here
    let cost = qty * pr
    tell $ Sum cost
    return (veg, pr)


addVegetable veg qty = do
    priceList <- lift ask   -- ← ask lives in ReaderT, lift brings it up to WriterT
    let pr = fromMaybe 0 $ lookup veg priceList
    let cost = qty * pr
    tell $ Sum cost
    return (veg, pr)

runMonads :: Vegetable -> Qty -> PriceList -> ((Vegetable, Price), Sum Cost)
runMonads veg qty pr = runIdentity $ runReaderT (runWriterT $ addVegetable veg qty) pr

type Name = String 
type Age = Int 
type Person = (Name, Age)
greet :: Person -> String 
greet (name, age) = "Hello" ++ show name ++ ", you are " ++ show age ++ " years old."


type Name' = String 
type Age' = Int 
type Person' = (Name' , Age')

greet' :: Person' -> String 
greet' (n, a) = "hello" ++ show n ++ "you are " ++ show a ++ " years old."

logNums :: [Int] -> Writer String Int
logNums [] = return 0
logNums (x:xs) = do
    tell "processed, "
    rest <- logNums xs
    return (x + rest)

type Env = Int 

-- Given this type:
-- type Env = Int  -- a multiplier from the environment

-- Complete this function:
-- It should read the multiplier from the environment,
-- multiply the input by it, and log the result
multnlog :: Int -> WriterT (Sum Int) (ReaderT Env Identity) Int 
multnlog x = do 
    env <- lift ask 
    tell $ Sum (x * env)
    return (x * env)

stInt :: State Int Int 
stInt = do 
    modify (+1)
    get 

stString :: State String String 
stString = do 
    modify (++"!!")
    gets (++"asd")
    put ("yo")
    get 

stComb :: StateT Int (StateT String Identity) (Int,String)
stComb = do 
    modify (+1)
    lift $ modify (++ "!!")
    a <- get 
    b <- lift get 
    return (a,b)


rComb = runIdentity $ evalStateT (evalStateT stComb 0) "a"

type MyStack a = StateT Int (StateT String Identity) a

q1 :: MyStack (Int,String)
q1 = do 
    modify (+5)
    lift $ modify (++"hey!")
    a <-get 
    b <- lift get 
    return (a,b)

type ThreeStack a = StateT Int (StateT String (StateT Bool Identity)) a 

flipBool :: ThreeStack (Int,String,Bool)
flipBool = do
    modify (+1)
    lift $ modify (++"yo!")
    lift $ lift $ modify not 
    a <- get 
    b <- lift get 
    c <- lift $ lift get 
    return (a,b,c)

type Garb a = WriterT String (StateT String (ReaderT Int Identity))a 

calcGarb :: Garb (String,String,Int)
calcGarb = do 
    (_, capturedLog) <- listen $ tell "editing writer"  -- spy on this tell
    -- tell (++ "editing writer") -- whats wrong ? 
    lift $ modify (++ "editing state")
    env <- lift $ lift ask 
    lift $ lift $ local  (+5) ask
    -- a <- listen calcGarb -- what should the argument be here ? 
    b <- lift get 
    c <- lift $ lift ask 
    return (capturedLog,b,c)

newtype MyMonadT m a 
    = MyMonadT { runMyMonadT :: m (MyMonad a) }
newtype MyMonad a = MyMonad { runMyMonad :: a }  -- a simple wrapper

-- instance Monad m => MonadFail (MyMonadT m) where 
--     fail s = lift . fail 

-- instance MonadTrans MyMonadT where 
    -- lift mx = ...

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a)}

instance MonadTrans MaybeT where 
    lift :: Functor m => m a -> MaybeT m a 
    lift = MaybeT . fmap Just 

instance Monad m => Monad (MaybeT m) where 
    return :: a -> MaybeT m a 
    return = MaybeT . fmap Just . return 

    (>>=) :: MaybeT m a -> (a-> MaybeT m b) -> MaybeT m b 
    mx >>= k = MaybeT $ do 
        v <- runMaybeT mx 
        case v of 
            Nothing -> return Nothing 
            Just y -> runMaybeT (k y)

instance Monad m => MonadFail (MaybeT m) where 
    fail :: String -> MaybeT m a 
    fail _ = MaybeT $ return Nothing

instance Monad m => Functor (MaybeT m ) where 
    fmap = liftM 
instance Monad m => Applicative (MaybeT m) where 
    pure = return 
    (<*>) = ap 

mbSt :: MaybeT (StateT Int Identity) Int 
mbSt = do 
    lift $ modify (+1)
    a <- lift get 
    True <- return $ a >=3 
    return a 

ex1 :: MaybeT Identity Int 
ex1 = do 
    x <- MaybeT (Identity (Just 4))
    y <-MaybeT (Identity Nothing)
    return (x + y)

-- odd double 
ex2 :: MaybeT (State Int) Int 
ex2 = do 
    lift $ modify (+1)
    x <- lift get 
    -- if even x then MaybeT (return Nothing)
    --             else return (x * 2)
    guard (odd x)
    return (x * 2)

-- **** Exercise 8: Implement EitherT (stretch) **** 
-- MaybeT gives you Nothing/Just
-- EitherT gives you Left error / Right value

-- Fill in this transformer:
{-newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance MonadTrans (EitherT e) where
    lift mx = EitherT (fmap Right mx)   -- hint given

instance Monad m => Monad (EitherT e m) where
    return x = ???
    mx >>= k = EitherT $ do
        v <- runEitherT mx
        case v of
            Left  e -> ???   -- propagate error
            Right a -> ???   -- continue
-}
-- -- Test: what should (return 5 >>= \x -> EitherT (return (Left "oops"))) return?

instance Monad m => Alternative (MaybeT m) where 
    empty = MaybeT $ return Nothing 
    x <|> y = MaybeT $ do 
        v <- runMaybeT x 
        case v of 
            Nothing -> runMaybeT y 
            Just _ -> return v 

instance Monad m => MonadPlus (MaybeT m)
    -- empty ahaha, can use guard now? 

instance MonadState s m => MonadState s (MaybeT m) where 
    --this is messed up , rather use lift to understand what happened. 
    get = lift get 
    put = lift . put 

mbSt'' :: MaybeT (State Integer) Integer
mbSt'' = do 
    modify (+1)   -- no lift needed!
    a <- get      -- no lift needed!
    guard $ a >= 3
    return a

safeTail :: MaybeT (State [Int]) [Int]
safeTail = do 
    xs <- get 
    guard $ not (null xs)
    return (tail xs)

tryPositive :: MaybeT (State Int) Int 
tryPositive = do 
    x <- get 
    guard $ x >0 
    return x 

tryNeg :: MaybeT (State Int) Int 
tryNeg = do 
    x <- lift get
    guard $ x < 0 
    return (abs x)

clac1 = tryPositive <|> tryNeg 

type Yaym a = MaybeT (State Int) a 

addPoints :: Int -> Yaym () 
addPoints n = modify (+n)

subPoints :: Int -> Yaym() 
subPoints n = do 
    score <- get 
    guard $ score - n >= 0 
    modify (subtract n)

playYayme :: Yaym Int 
playYayme = do 
    addPoints 30 
    subPoints 20 
    addPoints 2 
    return 69
