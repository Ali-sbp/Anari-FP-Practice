
import Control.Monad.Trans.Reader ( Reader, runReader, reader, ask, asks, local )
import Data.Maybe ( fromMaybe )
import Data.Char (toUpper, toLower)

greeting :: String -> String 
greeting = do 
    lower <- (map toLower)
    upper <- (map toUpper) 
    padded <- (\s -> "Mr. " ++ s)
    return (lower ++ " -> " ++ upper ++ " -> " ++ padded)

greeting' :: String -> String
greeting' = (map toLower) >>= (\x -> (map toUpper)
                            >>= \y -> (\s -> "Mr. " ++ s) 
                                    >>= (\z -> return (x ++ " " ++ y ++ " " ++ z)))

f :: Int -> Int 
f = (*2) >>= (\x -> (+10) 
                >>= (\y -> return (x - y)))                                    

double :: Reader Int Int 
double = reader (*2)            

append :: Reader String String 
append = reader (++ "asd")

arith :: Reader Int Int 
arith = reader $ (*2) >>= (\x -> (+3)
                    >>= (\y -> return (x + y)))
asd :: Reader String String 
asd = reader $ (++ " app1 ") >>= 
                (\y -> (++ " app2 ") >>= 
                        (\z -> return (y ++ z)))
showNum :: Reader Int String 
showNum = reader $ (*2) >>= (\x -> return ("number is half of : " ++ show x))                     

simpleR :: Show r => Reader r String 
simpleR = reader $ (\e -> "env is " ++ show e)

type User = String 
type Pass = String 
type UserTable = [(User, Pass)]

pwds :: UserTable
pwds = [("Bill","123"), ("Ann","qwerty"), ("John","2sRq8P")]

fstuser :: Reader UserTable User
fstuser = ask >>= (\x -> return $ fst (head x))

getpwlen :: User -> Reader UserTable Int 
getpwlen person = (asks $ lookup person) 
                    >>= (\e -> return (fromMaybe (-1) (fmap length e)))

doubleEnv :: Reader Int (Int ,Int )
doubleEnv = ask >>= (\e -> local (*2) ask >>=
                        (\x-> return (e, x)))

addPref :: Reader String (String,String,Int)
addPref = ask >>= (\e -> local ("yo, " ++ )ask  >>=
                    (\x-> return (e, x, 42))) 

userCunt :: Reader UserTable (Int,Int,Int) 
userCunt = asks length >>= (\e -> local (("Mike", "1") :)(asks length) >>=
                            (\x -> asks length >>=
                                (\y -> return (e,x,y))))                       