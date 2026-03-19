import Data.Semigroup as S 
import Data.List.NonEmpty as NE 
--import Data.Complex (imagPart)
import Data.Monoid as M 
--import qualified Data.Foldable as M
--import Lec04 (makeCakeMix)

--import Pr01_2 (myMaybe)
--import Data.Bits (Bits(xor))
--import GHC.IO.Device (SeekMode) 

newtype MaxOf a = MaxOf a deriving (Show,Eq,Ord)


instance Ord a => Semigroup (MaxOf a) where
    (<>) (MaxOf a) (MaxOf b) = if (MaxOf a) >= (MaxOf b) then MaxOf a else MaxOf b

newtype First' a = First' a deriving Show 
instance Semigroup (First' a) where 
    (<>) (First' a) (First' b) = First' a

data MyMaybe a = MyNothing | MyJust a deriving Show 

instance Semigroup a => Semigroup (MyMaybe a) where 
    (<>) MyNothing MyNothing = MyNothing
    (<>) MyNothing x = x
    (<>) x MyNothing = x 
    (<>) (MyJust a)(MyJust b)= MyJust (a <> b)

tst1 = sconcat (MyJust "a" :| [MyJust "b", MyJust "c"])
tst2 = stimes 3 (MyJust "ab")    -- should give MyJust "ababab"
tst3 = stimes 1 (MyJust "ab")    -- should give MyJust "ab"
tst4 = stimes 2 MyNothing :: MyMaybe String       -- should give MyNothing

tst5= mconcat ["Hello", " ", "World", "!"]
tst6= mempty :: [String]

tst7 = (S.getMin . M.mconcat . map S.Sum ) [1,2,3,4] :: Int 