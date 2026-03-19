import Data.Functor
import Data.Semigroup as S
import Data.Monoid as M
--import Foreign (FunPtr)
import Data.Foldable as FO(Foldable (toList))
--import Lec05 (MyName(MyName))
--import Text.Read (Lexeme(String))
--import GHC.IO.Device (IODevice(getSize))
--import Data.Maybe (mapMaybe)
tst1 = (<$> )(*2) [1,2,3]
tst2= (<&>) [1,2,3] (*3) 
tst3 = fmap ((*2) . (+3)) [1,2,3]
newtype Box a = Box a deriving Show
instance Functor Box where 
    fmap f (Box a) = Box (f a)
tst4 = fmap (+1) (Box 3)
--
data MyEither1 e a = MyL e | MyR a deriving (Show)
instance Functor (MyEither1 e ) where 
    fmap f (MyL e) = MyL e 
    fmap f (MyR a) = MyR (f a)
tst5 = fmap (+1) (MyL 2) 
-- tuple
newtype MyTuple a b = MyTuple (a, b) deriving (Show)
instance Functor (MyTuple x ) where 
    fmap f (MyTuple (a,b))  = MyTuple (a, f b)

fns = [Endo (+1), Endo (*2), Endo (+10)]
result1 = appEndo (mconcat fns) 0 
tst6= appEndo(mconcat [Endo (++ "!") , Endo(++ "Hello dumbass,")]) ""
tst7 = appEndo (mconcat [Endo (map (+1)) , Endo reverse]) [1,2,3,4,5]
tst8 = appEndo (mconcat [Endo (+1), Endo (*2), Endo (+3)]) 4 

data Pair a = Pair a a deriving Show 
instance Functor Pair where 
    fmap f (Pair x y) = Pair (f x) (f y)
tst9 = fmap (+1) (Pair 2 3) 

data MyEither2 e a = MyLeft2 e | MyRight2 a deriving Show 
instance Functor (MyEither2 e) where 
    fmap f (MyLeft2 x) = MyLeft2 x 
    fmap f (MyRight2 x) = MyRight2 (f x)
tst10 = fmap (+2) (MyLeft2 3)

data MyTree a = Leaf a | Node a (MyTree a)(MyTree a) deriving (Show,Eq)
instance Functor MyTree where 
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node x y z) = Node (f x) (fmap f y)(fmap f z) 

tstffs1 = fmap (+1) (Leaf 1)
tstffs2= fmap (+2) (Node 4 (Leaf 1) (Node 4 (Leaf 1)(Leaf 3)))

instance Foldable Pair where 
    foldMap f (Pair a b) = f a <> f b
tst11= foldr (+) 0 (Pair 3 5)
tst12= FO.toList (Pair 'a' 'b')

instance Foldable (MyEither1 e )  where 
    foldMap f (MyL e) = mempty
    foldMap f (MyR a) = f a 

tst13= foldr (+) 0 (MyL 1)
tst14= foldr (+) 0 (MyR 1)

instance Foldable MyTree where 
    foldMap f (Leaf a) = f a 
    foldMap f (Node a left right) = f a <> foldMap f left <> foldMap f right 
tst15 = FO.toList (Node 1 (Leaf 2) (Node 3 (Leaf 4) (Leaf 5)))
asd= Sum (Leaf 1)
tst16 = getSum $ foldMap Sum (Node 1 (Leaf 2) (Leaf 3)) 

--
newtype ZipList a = ZipList { getZipList :: [a] }

instance Functor ZipList where 
    fmap f (ZipList a) = ZipList (map f a )
instance Applicative ZipList  where
    pure x = ZipList (repeat x)               -- infinite list!
    ZipList gs <*> ZipList xs = ZipList (zipWith ($) gs xs)

tst17= getZipList $ ZipList [(2*),(3+),(4-)] <*> ZipList [1,2,3]
-- = [2*1, 3+2] = [2, 5]
-- (4-) has no pair, so it's dropped

tst18 = liftA2 max (Just 3)(Just 5)
tst19 = liftA2 max (Just 1 ) Nothing 

instance Applicative Pair where 
    pure x = Pair x x 
    (Pair f g) <*> (Pair x y) = Pair (f x) (g y)
tst20 = (Pair (+1) (*2)) <*> (Pair 4 5)

instance Applicative MyTree where 
    pure x  = Leaf x 
    Leaf f <*>  Leaf x = Leaf (f x)
    Node f g h <*> Node x left right = Node (f x) (g <*> left) (h <*> right)
    Leaf f <*> Node x left right = Node (f x)(fmap f left)(fmap f right)
    Node f g h <*> Leaf x = Node (f x)(g <*> pure x)(h <*> pure x)

tst21 =Leaf (+1) <*> Leaf 5
tst22 = pure 42 :: MyTree Int 
tst23 = Leaf (+10) <*> Node 1 (Leaf 2) (Leaf 3)
-- = Node 11 (Leaf 12) (Leaf 13)
tst24 = Node (+1) (Leaf (*2)) (Leaf (+10)) <*> Leaf 5
-- = Node 6 (Leaf 10) (Leaf 15)
tst25 = Node (+1) (Leaf (*2)) (Leaf (+10)) <*> Leaf 5
-- = Node 6 (Leaf 10) (Leaf 15)
tst26= fmap id (Node 1 (Leaf 2) (Leaf 3)) == Node 1 (Leaf 2) (Leaf 3)
-- = True
tst27 = liftA2 (+) (Leaf 3) (Leaf 4)
tst28 = liftA2 (+) (Node 1 (Leaf 2) (Leaf 3)) (Node 4 (Leaf 5) (Leaf 6))
-- = Node 5 (Leaf 7) (Leaf 9)
------

data MyMaybe a = MyNothing | MyJust a  deriving (Show,Eq,Ord)
instance Functor MyMaybe where 
    fmap f MyNothing = MyNothing
    fmap f (MyJust a) = MyJust (f a)

instance Applicative MyMaybe where 
    pure x = MyJust x 
    MyJust f <*> MyJust x = MyJust (f x)
    _ <*> MyNothing = MyNothing
    MyNothing <*> _ = MyNothing

tst30 = pure "asd" :: MyMaybe String
tst31 = MyJust (+1) <*> MyJust 1 

-- 
instance Applicative (MyEither2 e) where 
    pure x = MyRight2 x 
    MyLeft2 x <*> _ = MyLeft2 x 
    _ <*> MyLeft2 x = MyLeft2 x 
    MyRight2 f <*> MyRight2 x = MyRight2 (f x)