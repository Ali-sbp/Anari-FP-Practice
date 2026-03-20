module MyTypes.MyTree where
    
import Data.Functor as FO()
import Data.Monoid as M() 

data MyTree a = Leaf a | Node a (MyTree a)(MyTree a) deriving (Show,Eq)
instance Functor MyTree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node x y z) = Node (f x) (fmap f y)(fmap f z)

instance Foldable MyTree where
    foldMap f (Leaf a)         = f a
    foldMap f (Node a left right) = f a <> foldMap f left <> foldMap f right
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

---alternate application type : whole function Tree applied to each single leaf of values: 
data MyTree1 a = Leaf1 a | Node1 a (MyTree1 a)(MyTree1 a) deriving (Show,Eq)
instance Functor MyTree1 where 
    fmap f (Leaf1 a) = Leaf1 (f a)
    fmap f (Node1 x y z) = Node1 (f x) (fmap f y)(fmap f z) 
tstffs3 = fmap (+1) (Leaf1 1)
tstffs4= fmap (+2) (Node1 4 (Leaf1 1) (Node1 4 (Leaf1 1)(Leaf1 3)))
instance Applicative MyTree1 where
    pure x = Leaf1 x 
    Leaf1 f <*> valueTree = fmap f valueTree
    Node1 f leftFuncTree rightFuncTree <*> valueTree = 
        Node1 (f (root valueTree))
             (leftFuncTree <*> valueTree)
             (rightFuncTree <*> valueTree)
        where
            root (Leaf1 value) = value
            root (Node1 value _ _) = value

-- case: Leaf1 func <*> valueTree  ==>  fmap func over the whole tree
-- Leaf1 (*2) <*> Node1 3 (Leaf1 1) (Leaf1 4)  ==>  Node1 6 (Leaf1 2) (Leaf1 8)
tst1_leaf_func = Leaf1 (*2) <*> Node1 3 (Leaf1 1) (Leaf1 4)

-- case: Node1 func leftFuncTree rightFuncTree <*> valueTree
-- each subtree of funcs broadcasts over the entire valueTree
-- Node1 (+1) (Leaf1 (*2)) (Leaf1 (+10)) <*> Node1 5 (Leaf1 3) (Leaf1 7)
--   root:         (+1) applied to root of valueTree (5)        ==>  6
--   leftFuncTree: Leaf1 (*2) <*> entire valueTree              ==>  Node1 10 (Leaf1 6) (Leaf1 14)
--   rightFuncTree: Leaf1 (+10) <*> entire valueTree            ==>  Node1 15 (Leaf1 13) (Leaf1 17)
tst1_node_func = Node1 (+1) (Leaf1 (*2)) (Leaf1 (+10)) <*> Node1 5 (Leaf1 3) (Leaf1 7)

-- Applicative law: pure id <*> t == t
tst1_law_identity = (pure id <*> Node1 1 (Leaf1 2) (Leaf1 3)) == Node1 1 (Leaf1 2) (Leaf1 3)

-- Applicative law: pure f <*> pure x == pure (f x)
tst1_law_homomorphism = (pure (+1) <*> pure 5 :: MyTree1 Int) == pure (6 :: Int)
