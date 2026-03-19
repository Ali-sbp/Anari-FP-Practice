import Data.Monoid as M 
import Data.Semigroup as S 
import GHC.IO.Device (SeekMode)
import Data.Coerce as DC 
--import qualified Data.Foldable as M
--import Data.Vector.Unboxed (MVector(MV_2))
--import Control.Monad.ST (ST)


newtype MyString  = MyString  String deriving Show

instance Semigroup MyString  where 
    MyString a <> MyString b = MyString (a ++ b)
instance Monoid MyString where 
    mempty = MyString ""

newtype Counter = Counter Int deriving Show 

instance Semigroup Counter where 
    Counter a <> Counter b = Counter (a + b)
instance Monoid Counter where 
    mempty = Counter 0
data First1 a = Empty | Wrap a deriving Show 

instance Semigroup (First1 a) where 
    Wrap a <> Wrap b = Wrap a 
    Wrap a <> Empty = Wrap a 
    Empty <> Wrap a = Wrap a 
    Empty <> Empty = Empty 
instance Monoid (First1 a) where 
    mempty = Empty

tst1 = mconcat [Empty, Empty, Wrap 42, Wrap 6]

data Color = Red | Yellow | Blue | Green | Purple | Orange | Brown | Alpha deriving (Show, Eq,Ord,Enum)
instance Semigroup Color where 
    (<>) a b | a == b = a
             | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
             | all (`elem`[Blue, Yellow, Green])[a,b] = Green
             | all (`elem`[Red,Yellow,Orange]) [a,b] = Orange
             | a `elem` [Red .. Brown] && b == Alpha = a
             | b `elem` [Red .. Brown] && a ==Alpha = b 
             |otherwise = Brown
instance Monoid Color where 
    mempty = Alpha            

sameGp :: String -> String -> Bool 
sameGp a b | all (`elem` ["Cir", "Oval", "Elipse"]) [a,b]= True 
            | all (`elem`["Sq","Tri","Rect"]) [a,b] = False 
            |otherwise = False            

mergeZone :: Int -> Int -> String 
mergeZone a b | all (`elem` [-20..5]) [a,b] = "Cold af"
                | all (`elem`[15..25]) [a,b] = "warm"
                | all (`elem`[30..40]) [a,b] = "Hot"
                | all (`elem`[45..50])[a,b] = "Hell"
                |otherwise = "unknown zone"

grade :: Int -> String
grade n | n >= 90 = "A"
        | n >= 75 = "B"
        | n >= 60 = "C"
        |otherwise = "F"
classify :: Int -> String
classify n  | n `elem` [1,2,3] = "small"
            | n `elem` [4,5,6] = "Medium"
            | otherwise = "Big"

newtype Prod a = Prod  {getProduct :: a} deriving (Show,Eq,Ord,Enum)
instance Num a => Semigroup (Prod a) where 
    Prod a <> Prod b = Prod (a * b)
instance Num a => Monoid (Prod a) where 
    mempty = Prod 1
tst3 = mconcat [Prod 2,Prod 4,Prod 5]
    
newtype MaxOf a = MaxOf {getMaxOff :: a} deriving (Show,Eq,Ord)
instance Ord a => Semigroup (MaxOf a) where
    (<>) (MaxOf a) (MaxOf b) = if (MaxOf a) >= (MaxOf b) then MaxOf a else MaxOf b
instance (Ord a, Bounded a) => Monoid (MaxOf a) where 
    mempty = MaxOf minBound

---
data MyMaybe a = MyEmpty | MyJust a  deriving (Show,Eq,Ord)
instance Semigroup a => Semigroup (MyMaybe a) where
     MyEmpty <> MyJust a = MyJust a 
     MyJust a <> MyEmpty = MyJust a 
     (MyJust a) <> (MyJust b) = MyJust (a <> b)


instance Semigroup a => Monoid (MyMaybe a) where 
    mempty = MyEmpty
getMyMaybe :: MyMaybe a -> Maybe a
getMyMaybe MyEmpty    = Nothing
getMyMaybe (MyJust a) = Just a

tst10= (M.getSum  . mconcat . map Sum) [1, 2, 3]
tst11 = fmap M.getSum $ (getMyMaybe . mconcat . map MyJust) [Sum 1,Sum 2,Sum 3]
--
newtype Minn a = Minn {getMinn :: a} deriving (Show,Eq,Ord,Bounded,Num)
instance Ord a => Semigroup (Minn a) where 
    (<>) = coerce (min :: a -> a -> a)
    -- Minn a <> Minn b = Minn (min a b)
instance (Ord a, Bounded a) => Monoid (Minn a) where 
    mempty = maxBound
tst4 = Minn "hello" <> Minn "hi"
tst5 = (getMinn . mconcat . map Minn) [7,3,2,12] :: Int 
tst7=map Minn [1,2,3,4] 
tst6 = (S.getMin . M.mconcat . map S.Min) [1,2,3,4,5] :: Int 

newtype Person a = Person {getName :: a} deriving Show
newtype Temp = Temp {getTemp :: Int } deriving Show 

data MyEither e a = MyLeft e | MyRight a deriving (Show,Eq,Ord)
instance Semigroup (MyEither e a ) where 
    (<>)(MyLeft e)(MyRight a) = MyRight a 
    (<>)(MyRight a)(MyRight b) = MyRight a 
    (<>)(MyRight a)(MyLeft e) = MyRight a
    (<>)(MyLeft e)(MyLeft f) = MyLeft f 
tst12 = MyLeft "err1" <> MyRight 2
tst13 = MyRight 1 <> MyRight 3 
tst14 = MyLeft "err1" <> MyLeft "err2"
tst15=MyRight 12 <> MyLeft "err1"