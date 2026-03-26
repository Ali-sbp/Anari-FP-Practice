import Data.Foldable as FO
import Data.Monoid as M
import GHC.RTS.Flags (MiscFlags(installSEHHandlers))

data Drirection = North | South | East | West 

instance Show Drirection where 
    show North = "N"
    show South = "S"
    show East = "E"
    show West = "W"

opposite :: Drirection -> Drirection
opposite North = South
opposite South = North
opposite West = East
opposite East = West

instance Eq Drirection where 
    North == North = True
    South == South = True 
    East == East = True 
    West == West = True 
    _ == _ = False
    --x == y = show x == show y 

instance Enum Drirection where 
    toEnum 0 = North
    toEnum 1 = South
    toEnum 2 = East
    toEnum 3 = West
    fromEnum North = 0 
    fromEnum South = 1
    fromEnum East = 2 
    fromEnum West = 3

instance Bounded Drirection where 
    minBound = North
    maxBound = West
allDirs :: [Drirection]
allDirs = [minBound .. maxBound]

data MyEither e a = MyLeft e | MyRight a deriving (Show,Eq)

instance Foldable (MyEither e) where 
    foldr _ x (MyLeft y) = x
    foldr f z (MyRight x) = f x z
    --foldMap _ (MyLeft y) = mempty 
    --foldMap f (MyRight x) = f x

tst1 = getSum 

toList' :: MyEither e a -> [a]
toList' = foldr (:) [] 

tst2 = toList' (MyLeft "err")

data MyMaybe a = MyJust a | MyNothing deriving (Show,Eq)

instance Foldable MyMaybe where 
    foldr _ x MyNothing = x
    foldr f x (MyJust y) = f y x 

instance Semigroup a=> Semigroup (MyMaybe a) where    
    x <> MyNothing = x 
    MyNothing <> x = x
    (MyJust x) <> (MyJust y) = MyJust (x <> y)

instance Semigroup a => Monoid (MyMaybe a) where 
    mempty = MyNothing  

instance Functor MyMaybe where 
    fmap _ MyNothing = MyNothing 
    fmap f (MyJust x) = MyJust (f x)

instance Applicative MyMaybe where 
    pure x = MyJust x 
    MyNothing <*> _ = MyNothing 
    _ <*> MyNothing = MyNothing 
    (MyJust f) <*> (MyJust  y) = MyJust (f y)
    
myLiftA2' :: (a -> b -> c) -> MyMaybe a -> MyMaybe b -> MyMaybe c 
myLiftA2' f ma mb = (f <$> ma) <*> mb 

tst3 = myLiftA2' (+) (MyJust 2)(MyJust 3)

data MyTree a = Leaf a | Node a (MyTree a)(MyTree a) deriving (Show,Eq)

instance Foldable MyTree where 
   -- foldMap f 