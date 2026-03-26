module MyTypes.MyMaybe where
import Data.Functor as FO ()

data MyMaybe a = MyNothing | MyJust a  deriving (Show,Eq,Ord)

instance Foldable MyMaybe where
    foldr _ x MyNothing = x
    foldr f x (MyJust y) = f y x

tst1 = foldr (+) 0 (MyJust 5)
tst2 = foldr (+) 0 MyNothing
tst3 = foldMap show (MyJust 42)

instance Semigroup a => Semigroup (MyMaybe a) where

    -- MyNothing <> _ = MyNothing at first I made them this way but this breaks the monoid law. 
    -- _ <> MyNothing = MyNothing
    MyNothing <> x = x
    x <> MyNothing = x
    (MyJust x) <> (MyJust y) = MyJust (x <> y)

tst4 = MyJust "hi" <> MyJust "!"
tst5 = MyNothing <> MyJust [1,2]
tst6 = MyJust [1,2] <> MyJust [3]

instance Semigroup a => Monoid (MyMaybe a) where
    mempty = MyNothing

tst7 = (mempty :: MyMaybe [Int])
tst8 = mappend (MyJust "a") (MyJust "b")
tst9 = mconcat [MyJust "x", MyJust "y", MyNothing]

instance Functor MyMaybe where
    fmap f MyNothing = MyNothing
    fmap f (MyJust a) = MyJust (f a)

tst10 = fmap (*2) (MyJust 5)
tst11 = fmap (*2) MyNothing
tst12 = fmap show (MyJust 42)

instance Applicative MyMaybe where
    pure x = MyJust x
    MyJust f <*> MyJust x = MyJust (f x)
    _ <*> MyNothing = MyNothing
    MyNothing <*> _ = MyNothing

tst13 = (pure 5 :: MyMaybe Int)
tst14 = MyJust (+1) <*> MyJust 5
tst15 = MyNothing <*> MyJust 5

{-
-- Foldable: fold, foldMap, foldr
ghci> fold (MyJust [1,2,3])
[1,2,3]
ghci> fold MyNothing :: [Int]
[]
ghci> foldMap show (MyJust 42)
"42"
ghci> foldMap show MyNothing
""
ghci> foldr (+) 0 (MyJust 5)
5
ghci> foldr (+) 0 MyNothing
0

-- Semigroup: (<>), sconcat, stimes
ghci> MyJust "hi" <> MyJust "!"
MyJust "hi!"
ghci> MyNothing <> MyJust [1,2]
MyNothing
ghci> sconcat (MyJust "a" :| [MyJust "b", MyJust "c"])
MyJust "abc"
ghci> stimes 3 (MyJust "ha")
MyJust "hahaha"

-- Monoid: mappend, mconcat
ghci> mappend (MyJust "a") (MyJust "b")
MyJust "ab"
ghci> mappend MyNothing (MyJust "b") THIS WAS THE FIRST IMPLEMENTATION WHICH BREAKS MONOID LAWW !!!!!
MyNothing
ghci> mempty <> MyJust "asd" corrected version
MyJust "asd"
ghci> mconcat [MyJust "a", MyJust "b", MyNothing] SAME HERE !!!
MyNothing
ghci> mconcat [MyJust "a", MyJust "b", MyNothing] corrected version
MyJust "ab"
ghci> mconcat [MyJust [1], MyJust [2], MyJust [3]]
MyJust [1,2,3]

-- Functor: fmap, (<$)
ghci> fmap (*2) (MyJust 5)
MyJust 10
ghci> fmap (*2) MyNothing
MyNothing
ghci> 'x' <$ MyJust 5
MyJust 'x'
ghci> 'x' <$ MyNothing
MyNothing

-- Applicative: pure, (<*>), liftA2, (*>), (<*)
ghci> pure 5 :: MyMaybe Int
MyJust 5
ghci> MyJust (+1) <*> MyJust 5
MyJust 6
ghci> MyNothing <*> MyJust 5
MyNothing
ghci> liftA2 (+) (MyJust 3) (MyJust 4)
MyJust 7
ghci> liftA2 (+) MyNothing (MyJust 4)
MyNothing
ghci> MyJust 1 *> MyJust 2
MyJust 2
ghci> MyJust 1 <* MyJust 2
MyJust 1
-}
