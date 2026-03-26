module MyTypes.MyEither where

import Data.Foldable as FO()
import Data.Semigroup as S()


data MyEither e a = MyLeft e | MyRight a deriving Show

instance Foldable (MyEither e) where
    foldr _ x (MyLeft _) = x
    foldr f x (MyRight y) = f y x
    --foldMap _ (MyLeft _) = mempty
    --foldMap f (MyRight y) = f y 

tst1 :: Integer --wtf is this annoying warningggggg
tst1 = foldr (+) 0 (MyRight 5) 
tst2 = foldr (+) 0 (MyLeft "err")
tst3 = foldMap show (MyRight 42)

instance Semigroup a => Semigroup (MyEither e a) where
    MyLeft x <> _ = MyLeft x
    _ <> MyLeft y = MyLeft y
    MyRight x <> MyRight y = MyRight (x <> y)

tst4 = MyLeft "err" <> MyRight "b"
tst5 = MyRight "a" <> MyRight "b"
tst6 = MyRight [1,2] <> MyRight [3,4]

instance Functor (MyEither e) where
    fmap _ (MyLeft x) = MyLeft x
    fmap f (MyRight x) = MyRight (f x)

tst7 = fmap (+1) (MyRight 5)
tst8 = fmap (+1) (MyLeft "err")
tst9 = fmap show (MyRight 42)

instance Applicative (MyEither e) where
    pure x = MyRight x
    MyLeft e <*> _ = MyLeft e
    _ <*> MyLeft e = MyLeft e
    MyRight f <*> MyRight x = MyRight(f x)

tst10 = (pure 5 :: MyEither String Int)
tst11 = MyRight (+1) <*> MyRight 5
tst12 = (MyLeft "err" :: MyEither String (Int->Int)) <*> MyRight 5

{-
-- Foldable: fold, foldMap, foldr
ghci> fold (MyRight [1,2,3])
[1,2,3]
ghci> fold (MyLeft "err") :: [Int]
[]
ghci> foldMap show (MyRight 42)
"42"
ghci> foldMap show (MyLeft "err")
""
ghci> foldr (+) 0 (MyRight 5)
5
ghci> foldr (+) 0 (MyLeft "err")
0

-- Semigroup: (<>), sconcat, stimes
ghci> MyRight "a" <> MyRight "b"
MyRight "ab"
ghci> MyLeft "err" <> MyRight "b"
MyLeft "err"
ghci> sconcat (MyRight "a" :| [MyRight "b", MyRight "c"])
MyRight "abc"
ghci> stimes 3 (MyRight "ha")
MyRight "hahaha"

-- Functor: fmap, (<$)
ghci> fmap (+1) (MyRight 5)
MyRight 6
ghci> fmap (+1) (MyLeft "err")
MyLeft "err"
ghci> 'x' <$ MyRight 5
MyRight 'x'
ghci> 'x' <$ MyLeft "err"
MyLeft "err"

-- Applicative: pure, (<*>), liftA2, (*>), (<*)
ghci> pure 5 :: MyEither String Int
MyRight 5
ghci> MyRight (+1) <*> MyRight 5
MyRight 6
ghci> MyLeft "err" <*> MyRight 5
MyLeft "err"
ghci> liftA2 (+) (MyRight 3) (MyRight 4)
MyRight 7
ghci> liftA2 (+) (MyLeft "err") (MyRight 4)
MyLeft "err"
ghci> MyRight 1 *> MyRight 2
MyRight 2
ghci> MyRight 1 <* MyRight 2
MyRight 1
-}
