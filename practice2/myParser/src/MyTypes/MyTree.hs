module MyTypes.MyTree where
    
import Data.Functor as FO()
import Data.Monoid as M() 

data MyTree a = Leaf a | Node a (MyTree a)(MyTree a) deriving (Show,Eq)
instance Functor MyTree where 
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node x y z) = Node (f x) (fmap f y)(fmap f z) 
tstffs1 = fmap (+1) (Leaf 1)
tstffs2= fmap (+2) (Node 4 (Leaf 1) (Node 4 (Leaf 1)(Leaf 3)))
instance Applicative MyTree where 
    pure x  = Leaf x 
    Leaf f <*>  Leaf x = Leaf (f x)
    Node f g h <*> Node x left right = Node (f x) (g <*> left) (h <*> right)
    Leaf f <*> Node x left right = Node (f x)(fmap f left)(fmap f right)
    Node f g h <*> Leaf x = Node (f x)(g <*> pure x)(h <*> pure x)
tst21 =Leaf (+1) <*> Leaf 5
tst22 = pure 42 :: MyTree Int 
tst23 = Leaf (+10) <*> Node 1 (Leaf 2) (Leaf 3)

tst24 = Node (+1) (Leaf (*2)) (Leaf (+10)) <*> Leaf 5

tst25 = Node (+1) (Leaf (*2)) (Leaf (+10)) <*> Leaf 5

tst26= fmap id (Node 1 (Leaf 2) (Leaf 3)) == Node 1 (Leaf 2) (Leaf 3)

{-
-- Functor: fmap, (<$)
ghci> fmap (+1) (Leaf 1)
Leaf 2
ghci> fmap (*2) (Node 3 (Leaf 1) (Leaf 2))
Node 6 (Leaf 2) (Leaf 4)
ghci> 'x' <$ Leaf 1
Leaf 'x'
ghci> True <$ Node 3 (Leaf 1) (Leaf 2)
Node True (Leaf True) (Leaf True)

-- Applicative: pure, (<*>), liftA2, (*>), (<*)
ghci> pure 5 :: MyTree Int
Leaf 5
ghci> Leaf (+1) <*> Leaf 5
Leaf 6
ghci> Node (+1) (Leaf (*2)) (Leaf (+10)) <*> Leaf 5
Node 6 (Leaf 10) (Leaf 15)
ghci> liftA2 (+) (Leaf 3) (Leaf 4)
Leaf 7
ghci> Leaf 1 *> Leaf 2
Leaf 2
ghci> Leaf 1 <* Leaf 2
Leaf 1
-}

