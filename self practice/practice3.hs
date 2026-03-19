import GHC.IO.Encoding.Types (EncodeBuffer)
--import System.Posix (TerminalMode(EnableEcho))
--import Distribution.Simple (Bound)
--import Data.Time (Day)
import Data.List
import Data.Ord 
import Data.Semigroup (Semigroup)
import Data.Monoid (Monoid(mconcat, mempty))
--mport Lec04 (myConcat)
import Data.Either (Either(Right))
import Control.Monad (Functor)
--import Main (Rang(Sabz))
--import qualified Main as Red
--import qualified Main as Red
funC :: Int -> Int
funC x = 
    let x = 1 + 2 
    in x + x + x

a = seq 3 (funC 2)
--force eval
a1 = funC $! 2 

b1= [x^2| x <- [1..10], even x]

c1 = [ (x,y) | x <-[1,2,3], y <- ['a','b','c','d'] ]

d1 = [(a,b,c) | a<- [1..20] , b <- [1..a],c <- [1..20], a^2 + b^2 == c^2]

e1 = [(a,b) | a<- [1..10], b <- [1..a]]
f1 = [ x | x <- [1..30], mod x 3 == 0]
g1 = [(x,y) | x <- [1..4],y <- [1..4] , x < y]
h1 = [x | x <- ['A'..'z'] , x == 'S']

class Anari a where 
    describe :: a -> String

data Season = Spring | Summer | Autumn | Winter 

instance Anari Season where 
    describe Spring = "green mood"
    describe Summer = " yellow mood"
    describe Autumn = "orange mood"
    describe Winter = "gray mood"

data TraficLight = Red | Yellow | Green 

instance Show TraficLight where 
    show Red = "Red"
    show Yellow = "Yellow"
    show Green = "Green"
class Show1 a where 
    show1 :: a -> String

class Eq1 a b  where 
    (*=) :: a -> b -> Bool 
data Fruits = Khiar | Havij | Moz 
data Rang = Sabz | Narenji | Zard 
instance Eq1 Fruits Rang where 
    (*=) :: Fruits -> Rang -> Bool
    Khiar *= Sabz = True
    Havij *= Narenji = True 
    Moz *= Zard = True 
    _ *= _ = False

{-class Eq2 a where 
    (==) :: a-> a -> Bool
    (/=) :: a -> a -> Bool
instance Eq2 Fruits where 
    Moz == Moz = True
    Khiar == Khiar = True
    Havij == Havij = True
    _ == _ = False
-}
instance Eq Fruits where 
    Moz == Moz = True 
    Khiar == Khiar = True 
    Havij == Havij = True 
    _ == _ = False 
class Eq a => Ord1 a where 
    compare1 :: a -> a -> Ordering -- LT | EQ | GT

instance Ord Fruits where 
    compare Moz Moz = EQ 
    compare Moz _ = GT 
    compare Khiar Khiar = EQ 
    compare Khiar _ = GT 
    compare Havij Havij = EQ 
{-class Enum a where 
    toEnum :: Int -> a 
    fromEnum :: a -> Int -}
instance Enum TraficLight where 
    fromEnum Red = 0
    fromEnum Green = 1 
    fromEnum Yellow = 2 
    toEnum 0 = Red
    toEnum 1 = Green
    toEnum 2 = Yellow
a12= [Red .. Yellow]
instance Bounded TraficLight where 
    minBound = Red 
    maxBound = Yellow 

instance Read TraficLight where 
    readsPrec _ str = case str of 
        'R':'E':'D' : rest -> [(Red , rest)]
        'G':'R':'E':'E':'N' : rest -> [(Green, rest)]
        'Y':'E':'L':'L':'O':'W' : rest -> [(Yellow, rest)]
        _ -> []

test1= read "RED" :: TraficLight

data Rang1 = Sabzz | Zardd | Ghermez | Abi 
    deriving (Show, Read, Eq , Ord, Enum, Bounded)
test2 = 1 + (fromEnum Ghermez)
test3 = Abi > Ghermez

-- myAnotherDie
-- succ myAnotherDie
-- myAnotherDie > S5
-- sort [myAnotherDie, S5, S1]
-- (read "S1") :: SixSidedDie
--How to use 
abi= Abi
gher=Ghermez
zar=Zardd

--Enum : pred , succ , [A..B]
--sort ???

data DayOfWeek = Mon | Tue | Wed 

instance Show DayOfWeek where 
    show Mon = "Monday"
    show Tue = "Tuesday"
    show Wed = "Wednesday"
instance Eq DayOfWeek where
    Mon == Mon = True
    Tue == Tue = True
    Wed == Wed = True 
    _ == _ = False 
instance Enum DayOfWeek where 
    fromEnum Mon = 0 
    fromEnum Tue = 1 
    fromEnum Wed = 2
    toEnum 0 = Mon 
    toEnum 1 = Tue 
    toEnum 2 = Wed 
instance Bounded DayOfWeek where 
    minBound = Mon
    maxBound = Wed 

data DayOfWeek' = Mon' | Tue' | Wed'
    deriving(Show, Eq, Ord, Read, Enum, Bounded)

class Semigroup a where 
    (<>) :: a -> a -> a

instance Semigroup TraficLight where 
    Red <> _ = Red
    _ <> Red = Red 
    Yellow <> _ = Yellow
    _ <> Yellow = Yellow
    Green <> Green = Green

myMconcat :: Monoid a => [a] -> a  -- function type??
myMconcat = foldr (<>) mempty
 

--newtype ********
newtype Sum a = Sum {getSum:: a}
newtype Product = Product {getProduct :: a} 

myFmap :: (a->b) -> f a -> f b 

--f <$> container = myFmap f container  

a = fmap (+1) (Left 2) 
b = fmap (+1) (Right 2) -- why it changes and left doesnt??


data Box a = Box as
newtype BBox a = BBox a 

instance Functor Box where 
    fmap f (Box a) = Box (f a)

instance Functor BBox where 
    fmap f (BBox a) = BBox (f a)

data Color = Red | Yellow | Blue | Green | Purple | Orange | Brown | Alpha deriving (Show, Eq)

instance Semigroup Color where
  (<>) Red    Blue    = Purple
  (<>) Blue   Red     = Purple
  (<>) Yellow Blue    = Green
  (<>) Blue   Yellow  = Green
  (<>) Yellow Red     = Orange
  (<>) Red    Yellow  = Orange

  (<>) Red    Alpha   = Red 
  (<>) Yellow Alpha   = Yellow 
  (<>) Blue   Alpha   = Blue 
  (<>) Green  Alpha   = Green 
  (<>) Purple Alpha   = Purple 
  (<>) Orange Alpha   = Orange 
  (<>) Brown  Alpha   = Brown 
  (<>) Alpha  Red     = Red 
  (<>) Alpha  Yellow  = Yellow 
  (<>) Alpha  Blue    = Blue 
  (<>) Alpha  Green   = Green 
  (<>) Alpha  Purple  = Purple 
  (<>) Alpha  Orange  = Orange 
  (<>) Alpha  Brown   = Brown 

  (<>) a b = if a == b then a else Brown   -- выполняется ли закон Semigroup?
                                            -- проверяется руками
                                        
                

