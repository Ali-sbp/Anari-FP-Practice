import Control.Monad.Trans.Writer ( Writer, writer, runWriter, execWriter, writer, censor, listen, listens, tell )
import Data.Maybe ( fromMaybe )
import Data.Char (toUpper)
import Data.Monoid ( Sum (..), Product )
import GHC.Read (list)

stepA :: Int -> (Int,String)
stepA n = (n*2 ,"doubled")

stepB :: Int -> (Int , String)
stepB n = (n+10 , "plust10 ")

-- chainAB :: Int -> (Int, String)
-- chainAB n = (stepA) >>= (\x -> (stepB ) 
--                             (\y -> return ()))

chainAB' :: Int -> (Int, String)
chainAB' n = 
    let (a , log1) = stepA n 
        (b , log2) = stepB a 
    in (b, log1 ++ "; " ++ log2)
w1 :: Writer String Int 
w1 = writer (42,"Asd")

act1 :: Int -> Writer String Int 
act1 n = writer (n, "initial input was " ++ show n ++ "; ")

act2 :: Int -> Writer String Int 
act2 n = writer (n *2 , "doubled " ++ show n ++ ";")

res :: Int -> Writer String Int 
res n = act1 n >>= act2 

w2 :: Writer String Int 
w2 = writer (3 , "step1; ") >>= (\x -> writer (x+1, "step2; "))

w3 :: Writer (Sum Int) Int 
w3 = writer (3, Sum 10) >>= \x -> writer (x*2 , Sum 5)

w4 :: Writer [Int] String 
w4 = writer ("a", [1,2]) >>= \x -> writer (x++"b", [3,4])

tellexample :: Writer String Int 
tellexample = tell "start; " >>= (\x -> tell "middle; " >>=
                                    \y -> return 42)

countshit :: Writer (Sum Int) String 
countshit = tell (Sum 3) >>= (\x -> tell (Sum 5) >>= 
                                \y -> return "done")

wtf :: Writer String Int 
wtf = tell "step1; " >>= \x -> return 99

actualwtf :: Writer String (Int,String)
actualwtf = listen wtf 

myact1 :: Writer (Sum Int) String 
myact1 = tell (Sum 42) >>= \x -> return "result"

withlistens = listens getSum myact1

asd1 :: Writer String Int 
asd1 = tell "asd" >>= \x-> return 2 
asd2 = listens (++ "!!") asd1                         

action :: Writer String Int 
action = tell "hello ape" >>= \x -> return 5
action2 = censor (map toUpper) action 

dumbshit :: Writer (Sum Int) Bool 
dumbshit = tell (Sum 3) >>= \x -> return False 

dumbshit2= censor (\(Sum x) -> Sum (x*2)) dumbshit

ex2 :: Writer (Sum Int) (Int, Sum Int)
ex2 = listen $ do
    tell (Sum 5)
    return 10

hmm :: Writer (Sum Int) (Int, Sum Int)
hmm = listen $ tell (Sum 3) >>= \x -> return 4

listensprac :: Writer String (Int,String)
listensprac = listens (++ "!! ") $ tell "asd; " >>= \x -> return 2 

listen1 :: Writer String (Int,String)
listen1 = listen $ tell "hey" >>= \x -> return 2

listens1 :: Writer String (Int,String)
listens1 = listens (map toUpper) $ 
            censor(++ "!!") $
                censor (++ "&&") $
                    tell "hey" >>= \x -> return 2 

type Vegetable = String
type Price     = Double
type Qty       = Double
type Cost      = Double    -- Cost = Qty * Price
type PriceList = [(Vegetable, Price)]
prices :: PriceList
prices = [("Potato", 13), ("Tomato", 55), ("Apple", 48)]

addVegetable :: Vegetable -> Qty -> Writer (Sum Cost) (Vegetable, Price)
addVegetable veg qty = do
    let pr   = fromMaybe 0 $ lookup veg prices  -- look up price, default 0
    let cost = qty * pr                           -- compute cost
    tell $ Sum cost                               -- LOG the cost
    return (veg, pr)                              -- RETURN (name, price)

mycart :: Writer (Sum Cost) [(Vegetable,Price)]
mycart = addVegetable "Potato" 3.0 >>= 
            (\x -> addVegetable "Tomato" 1.0 
            >>= (\y-> addVegetable "Apple" 1.0 
            >>= (\z-> return [x,y,z])))

mycart1 :: Writer (Sum Cost) [((Vegetable, Price), Sum Cost)]
mycart1 = (listen $ addVegetable "Potato" 2.0) >>=
             (\x -> (listen $ addVegetable "Tomato" 4.0) >>=
                \y-> return [x,y])
mycart1' :: Writer (Sum Cost) [((Vegetable, Price),Cost)]
mycart1' = (listens getSum $ addVegetable "Potato" 2.0) >>=
             (\x -> (listens getSum $ addVegetable "Tomato" 4.0) >>=
                \y-> return [x,y])

discount :: Double -> Sum Cost -> Sum Cost
discount proc s = case s of
    Sum x
        | x < 100   -> s              -- no discount if total < 100
        | otherwise -> Sum $ x * (100 - proc) / 100   -- apply % discount

myCart0' :: Writer (Sum Cost) [(Vegetable, Price)]
myCart0' = censor (discount 10) mycart 
