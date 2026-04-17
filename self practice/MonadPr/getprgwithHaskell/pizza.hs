module Pizza where 
import System.Random
import Data.String 
import Data.Char(digitToInt)
-- import Text.Parsec (Parsec, parse, digit, char, many1,try,ParseError,(<|>))
-- import Text.Parsec.String (Parser)

minDie :: Int
minDie = 1
maxDie :: Int
maxDie = 6
main :: IO ()
main = do
    putStrLn " Hello , size of pizza 1 ?" 
    sizep1 <- getLine 
    putStrLn " Hello , cost of pizza 1 ?" 
    costp1 <- getLine
    putStrLn " Hello , size of pizza 2 ?" 
    sizep2 <- getLine
    putStrLn " Hello , cost of pizza 2 ?" 
    costp2 <- getLine
    let pizza1 = (read sizep1, read costp1)
    let pizza2 = (read sizep2, read costp2)
    let betterPizza = comparePizzas pizza1 pizza2
    putStrLn $ show (read sizep1 :: Int)
    putStrLn (describePizza betterPizza)
    

helloperson :: String -> String 
helloperson name = "Hello " ++ " " ++ name ++ "!!"

-- digits :: Parser Int 
-- digits = foldl(\x d -> x*10 + d) 0  <$> many1(digitToInt <$>  digit )

areaGivenDiameter :: Double -> Double
areaGivenDiameter size = pi*(size/2)^2

type Pizza = (Double,Double)

costPerInch :: Pizza -> Double
costPerInch (size, cost) = cost / areaGivenDiameter size

comparePizzas :: Pizza -> Pizza -> Pizza
comparePizzas p1 p2 = if costP1 < costP2
    then p1
    else p2
        where costP1 = costPerInch p1
              costP2 = costPerInch p2

describePizza :: Pizza -> String
describePizza (size,cost) = "The " ++ show size ++ " pizza " ++
                            "is cheaper at " ++
                            show costSqInch ++  
                            " per square inch"
                            where costSqInch = costPerInch (size,cost)       

