import Control.Monad.State ( State, state, execState, runState, evalState, get, gets, put, modify )
import Control.Monad ( replicateM )


tick1 :: Int -> (Int,Int)
tick1 n = (n, n+1)

chainticks = let (a, s1) = tick1 5
                 (b, s2) = tick1 s1 
                 (c, s3) = tick1 s2 
    in (a,b,c)

addOne :: State Int ()
addOne = modify (+3)

result = execState (addOne >> addOne) 2 

doubleS :: Int -> (Int , Int)
doubleS s = (s*2, s +1)

chainD = let (a, s1) = doubleS 3
             (b, s2) = doubleS s1 
        in (a,b)

state1 :: State Int Int 
state1 = state (\s -> (s,s))

idstate :: State Int Int 
idstate = state (\x -> (x, 69))
reset :: State Int ()
reset = state (\_ -> ((), 0))

ch1 = state1 >>= \x -> reset 

double' :: State Int Int 
double'= state (\s -> (s*2 , s))

greet :: State String String 
greet = state (\s -> ("hello" ++ s, s))

myComp :: State Int Int 
myComp = state (\s-> (s*2, s+1))

trip :: State Int () 
trip = state (\s -> ((), s * 3))

twoRets :: State Int (Int,Int)
twoRets = return 5 >>= (\x -> return 10 
                    >>= (\y -> return (x,y)))

comp :: State String Int 
comp = do 
    x <- return 42 
    return x 
comp' :: State String Int 
comp' = state (\st -> (42, st))

sec51:: State Int ()
sec51 = (put 0)

sec52 :: State Int Bool 
sec52 = gets (>10) 

sec53 :: State Int () 
sec53 = do 
    s <- get
    put (s*2)
    modify (+1)

tick :: State Int Int
tick = get >>= (\s -> put (s+1) >> return s) 

tick' :: State Int Int 
tick' = get >>= (\n -> put (n+1) >>= \_ -> return n)

preTick :: State Int Int 
preTick = get >>= (\n -> put (n+1) >>= \_ -> return (n+1) )

doubltick = tick >>= (\s-> tick >>= \_ -> return ())

succ' :: Int -> Int 
succ' n = execState tick n 

plus :: Int -> Int -> Int 
plus n x = execState (sequence $ replicate n tick) x 

plus' :: Int -> Int -> Int 
plus' n x = execState (replicateM n tick) x 