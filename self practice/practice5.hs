--import GHC.Base (TrName(TrNameD))
--import Control.Concurrent (yield)
--import Text.ParserCombinators.ReadP (count)
--import Pr01_2 (Shape(Circle))
type Name = String 
type Point = (Double, Double)
data Circle = Circle Point Double
            deriving Show

circle1 = Circle (2.0 , 1.0) 4.0 

newtype BetterName = BetterName String
newtype Point1 = Point1 (Double, Double)

class Deb a where
    describe :: a -> String

instance Deb Point1 where
    describe (Point1 (x, y)) = "this is a point with x = " ++ show x ++  " and y = " ++ show y 

instance Deb Circle where
    describe (Circle (x,y) r) = "this is a Circle with Center of : (" ++ show x ++ ", " ++ show y ++ ") and radius of : " ++ show r 


type FirstName = String
type LastName = String 
type Greeting = String 

greet :: FirstName -> LastName -> Greeting
greet f l = "Hello, " ++ f ++ " " ++ l ++ "!"

type Matrix1 = [[Integer]]
matrixSize :: Matrix1 -> (Int, Int)
matrixSize x = (length(x), length(head x))

mat1 = [[1,2,3],
        [4,5,6]]

type Grid = [[Bool]]
gridDimentions :: Grid -> (Int, Int)
gridDimentions x = (length x , length(head x))

grid1 = [[True, True], 
         [False , True]]

countTrue :: Grid -> Int
countTrue x = length (filter (==False) (concat x))

tstCount = countTrue grid1
test1 = concat [[1,2], [3,4], [5]] 
test2 = filter (>2) test1

rowSum :: Matrix1 -> [Integer]
rowSum x = map sum x 

tstSum = rowSum mat1


data Seasons = Spring | Summer | Fall | Winter deriving (Eq,Enum, Ord)
nestSeason :: Seasons -> Seasons
nestSeason s1 = succ(s1)

isWarm :: Seasons -> Bool
isWarm Summer = True
isWarm Winter = False
isWarm Fall = False
isWarm Spring = True

data TL = Red | Yellow | Green deriving (Show, Enum, Ord)
nxtLight :: TL -> TL
nxtLight x = succ x

instance Show Seasons where 
    show Spring = "🌱 Spring"
    show Summer = "☀️ Summer"
    show Fall =  "🍂 Autumn"
    show Winter = "❄️ Winter"

instance Eq TL where 
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True

data Shape = Cir Int | Rec (Int, Int) | Tri(Int,Int,Int)
instance Show Shape where 
    show (Cir x) = "Circle with Radius " ++ show x
    show (Rec (x,y)) = "Rect with " ++ show x ++ ", " ++ show y
    show (Tri (x,y,z)) = "Tri with "++ show x ++  ", " ++ show y ++ show ", " ++ show z 

sa = Cir 1 ::Shape 
newtype Lat = Lat Int deriving Show
newtype Lon = Lon Int deriving Show
moveShip :: Lat -> Lon -> String 
moveShip la lo = "ship moved to " ++ show la ++ ", " ++ show lo

newtype StuID = StuID Int 
instance Show StuID where 
    show (StuID x) = "ID#"++ show x 
stu1= StuID 2321

class Revable a where 
    rev :: a -> a 

instance Revable [a] where
    rev [] = []
    rev (x:xs) = (rev xs) ++ [x]  
instance Revable Seasons where 
    rev Summer = Winter
    rev Fall = Spring
    rev Spring = Fall 
    rev Winter = Summer

class Cyclic a where 
    next :: a -> a
    prev :: a -> a 
instance Cyclic Seasons where 
    next Spring = Summer
    next Summer = Fall
    next Fall = Winter
    next Winter = Spring 
    prev Spring = Winter
    prev Winter = Fall
    prev Fall = Summer 
    prev Summer = Spring 