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
                                        
data Asd a = Asd a deriving Show
instance Functor Asd where 
    fmap f (Asd a) = Asd (f a)
s = fmap show (Asd 2)