-------------------------------------
-- Практические задание 1. Часть 2 --
-------------------------------------

module Pr01_2 where
import GHC.Base (BCO, DoubleBox)
import Distribution.Simple.LocalBuildInfo (prefixRelativeComponentInstallDirs)
--import Lec04 (Ingredients(BakingPowder))
--import Lec04 (CakeDough(ChocolateCakeDough))



{-

Напишите реализацию функций:
-- myZipSave - попарное объединение двух списков в список пар и сохранение хвоста более длинного списка 
-- myUnzipSave - разделение списка пар на пару списков с восстановлением более длинного списка если исходные списки были разного размера
-- myReverse - разворот списка с использованием сверток
-- myFoldl1 - левая свертка для не пустых списков (без инициирующего значения)
-- myFoldr1 - правая свертка для не пустых списков (без инициирующего значения)
-- myTakeWhile - реализовать с использованием сверток
-- mySpan - реализовать с использованием сверток
-- myMaybe - обработка возможно отсутствующего значения или возвращение значение по умолчанию (maybe)
-- myMap - реализуйте функцию map с использованием типа MyList из материалов лекции
-- myUnFoldr - развертка (операция обратная к свертке)

-- Расширьте типы для выпекания тортов из материалов лекции:
    -- Добавить возможность испечь не менее трех типов тортов
    -- Контроль числа и объема используемых ингредиентов
    -- Обработку недостатка или отсутствия ингредиентов

-}

myZipSave :: [a] -> [b] -> ([(a,b)] , Maybe (Either [a] [b]))
myZipSave [] [] = ([] , Nothing) 
myZipSave (x: xs) [] = ([] , Just (Left (x:xs)))
myZipSave [] (y:ys) = ([] , Just (Right (y:ys)))
myZipSave (x:xs) (y:ys) = 
    let (pairs , leftover) = myZipSave xs ys
    in ( (x,y): pairs , leftover)

-- question : is this normal that in test , it prints "Just" and "right" ? 
--ghci> myZipSave [1,2,3] [4,5,6,7,8]
-- ([(1,4),(2,5),(3,6)],Just (Right [7,8]))

myUnzipSave :: Maybe (Either [a] [b]) -> [(a,b)] -> ([a] , [b])
myUnzipSave Nothing  [] = ([],[])
myUnzipSave Nothing ((a,b) : rest) = 
    let (as , bs ) = myUnzipSave Nothing rest 
    in (a: as , b : bs )
myUnzipSave (Just (Left (x:xs))) [] = ((x:xs), [])
myUnzipSave (Just (Right (y:ys))) [] = ([] , (y:ys))
myUnzipSave (Just (Left (x:xs))) ((a,b) : rest) = 
    let (as , bs) = myUnzipSave (Just (Left (x:xs))) rest 
    in ( a: as , b : bs )
myUnzipSave (Just (Right (y:ys))) ((a,b): rest) = 
    let (as , bs) = myUnzipSave (Just (Right (y:ys))) rest
    in (a : as , b : bs )

--this looks really stupid , the implementation, should it be this way? 

myFoldl :: (b -> a -> b) -> b -> [a] -> b 
myFoldl f b [] = b 
myFoldl f b (x:xs) = myFoldl f (b `f` x) xs

myReverse :: [a] -> [a]
myReverse xs = myFoldl (\f x -> x : f) [] xs 

--myReverse = myFoldl (\f x -> x : f) [] eta reduction suggested by extention?


--myFoldl1
myFoldl1 :: (a->a->a) -> [a] -> a 
myFoldl1 f [] = error " asd "
--myFoldl1 f [a] = a 
myFoldl1 f (x:xs) = jhonny x xs
    where
        jhonny acc []= acc 
        jhonny acc (y:ys) = jhonny (f acc y) ys 
--TODO1 done 
myFoldl12 :: (a -> a -> a) -> [a] -> a
myFoldl12 f []       = error "empty list"
myFoldl12 f [x]      = x
myFoldl12 f (x:y:ys) = myFoldl12 f (f x y : ys)


--myFoldr , wasted 2 hours and ended up not using it in the fucking implementation!
myFoldr :: (a -> b -> b) -> b -> [a] -> b 
myFoldr f b [] = b
myFoldr f b (x:xs) = x `f` (myFoldr f b xs)  
--myFoldr1

myFoldr1 :: (a -> a -> a) -> [a] -> Maybe a
myFoldr1 f [] = Nothing
myFoldr1 f [a] = Just a
myFoldr1 f (x : xs) = 
    let Just z = myFoldr1 f xs
    in Just (f x z) 


--myTakeWhile without folds

mytakeWhile :: (a -> Bool) -> [a] -> [a] 
mytakeWhile p [] = [] 
mytakeWhile p (x:xs) 
    | p x = x : mytakeWhile p xs
    | otherwise = []

--mytakeWhile with folds

mytakeWhile2 :: (a -> Bool) -> [a] -> [a] 

--mytakeWhile2 p [] = []
mytakeWhile2 p list = foldr localF [] list
    where 
        localF x rest 
            | p x = x : rest 
            | otherwise = [] 

--mySpan 

mySpan :: (a-> Bool) -> [a] -> ( [a], [a])
mySpan p xs = foldr localF ([],[]) xs
    where 
        localF z (xs , ys) 
            |p z = (z:xs , ys)
            |otherwise =  ([] , z: xs ++ ys)


--myMaybe

myMaybe :: b -> (a -> b) -> Maybe a -> b
myMaybe n f Nothing = n 
myMaybe n f (Just v) = f v 


--myMap 
data MyList a = MyEmpty | MyCons a (MyList a) deriving Show 

myMap :: (a->b) -> MyList a -> MyList b
myMap _ MyEmpty = MyEmpty
myMap f (MyCons x xs) = MyCons (f x) (myMap f xs)

asd= myMap (+1) (MyCons 1 (MyCons 2 (MyCons 3 MyEmpty)))

--myUnfoldr

myUnfoldr :: (b -> Maybe (a , b)) -> b -> [a]
myUnfoldr f b = myMaybe [] (\(x, next) -> x : myUnfoldr f next) (f b)

--random test case
asd2 = myUnfoldr (\n -> if n == 0 then Nothing else Just (n, n-1)) 5


--types 
--random practice
data Drink = Tea | Coffee | Juice 

drinkTemp :: Drink -> String 
drinkTemp Tea = "hot"
drinkTemp Coffee = "hot"
drinkTemp Juice = "cold"

data Planet = Saturn | Jupiter | Earth
hasrings :: Planet -> Bool
hasrings Saturn= True 
hasrings Jupiter = True
hasrings Earth= False


data Shape = Circle Double | Rect Double Double

perimeter :: Shape -> Double 
perimeter (Circle r) = 3.14 * 2 * r
perimeter (Rect w h) = w*2 + h*2 

--pizza : 
data Base = Thinbase | ThickBase deriving Show
data Sause = Tomatosause | Creamsause deriving Show
data Pizza = Margarita | Pepperoni | Vegge deriving Show

makePizza :: Maybe Base -> Maybe Sause -> Maybe Pizza
makePizza (Just Thinbase) (Just Tomatosause) = Just Margarita
makePizza (Just ThickBase) (Just Tomatosause) = Just Pepperoni
makePizza (Just ThickBase) (Just Creamsause) = Just Vegge
-- makePizza (Just Thinbase) (Just Creamsause) = Nothing -- redundant
--how to write anything else = Nothing ?
makePizza _ _ = Nothing 

describePizza :: Maybe Pizza -> String
describePizza (Just Vegge) = "thick base + creamsause"
describePizza (Just Margarita) = " Thinbase + tomato sause"
describePizza (Just Pepperoni) = "thicbase + tomatosauce"
describePizza Nothing = "Nothing?"
--how to write any other? 
--can use this cause pattern checking is top to bottom!


--cakes 
--commenting it for less headache
{-
data Cake = ChocolateCake | BananaCake | CherryCake deriving Show 
data Ingredients = Oil | Chocolate | Egg | Flour | Sugar | Bakingpowder | Banana | Cherry deriving Show
data Fillingmix = Oilchocolatemix | Bananmix | Cherrymix deriving Show
data Dough = CakeDough deriving Show
data Cakedough = ChocolateDough | BananaDough | CherryDough deriving Show 


-- Функции, которые описывают процесс приготовления частей торта
makeCakeMix ::  Ingredients ->  Ingredients ->  Fillingmix
makeCakeMix  Oil Chocolate = Oilchocolatemix
makeCakeMix Chocolate Oil = Oilchocolatemix
-- ...
makeCakeMix Banana Oil = Bananmix 
makeCakeMix Oil Banana = Bananmix 
makeCakeMix Cherry Sugar = Cherrymix 
makeCakeMix Sugar Cherry = Cherrymix

cakeDough :: Ingredients -> Ingredients -> Ingredients -> Ingredients -> Dough
cakeDough Egg Flour Sugar Bakingpowder = CakeDough
-- ...

--i will make a new one
{-
chocolateCakeDough :: Dough -> Fillingmix -> Cakedough
chocolateCakeDough CakeDough Oilchocolatemix = ChocolateDough
-- ...-}
cakeDoughFinal :: Dough -> Fillingmix -> Cakedough
cakeDoughFinal CakeDough Oilchocolatemix = ChocolateDough 
cakeDoughFinal CakeDough Bananmix = BananaDough
cakeDoughFinal CakeDough Cherrymix = CherryDough

--will also make a new one:
--Also error on not caps on "c" ? it will make it a variable wwhich will match anything?
{-
chocolateCake :: Cakedough -> Action -> Cake
chocolateCake ***c***hocolateCakeDough Bake = ChocolateCake
-- ... -}
data Action = Bake deriving Show
makeCake :: Cakedough -> Action ->Cake
makeCake ChocolateDough Bake = ChocolateCake
makeCake BananaDough Bake = BananaCake
makeCake CherryDough Bake = CherryCake
-}
--examples 
data Stock = Apples Int | Milk Double deriving Show 
useApples :: Int -> Stock -> Maybe Stock
useApples m (Apples n) 
    | n > m = Just (Apples (n-m))
    |otherwise = Nothing  

useApples _ _ = Nothing 

data Ingredients = Oil Double 
                |Chocolate Double
                |Egg Int
                |Flour Double
                |Sugar Double
                |Bakingpowder Double
                |Banana Int
                |Cherry Int 
                deriving Show 

data Cake = ChocolateCake | BananaCake | CherryCake deriving Show 
data Fillingmix = Oilchocolatemix | Bananamix | Cherrymix deriving Show
data Dough = CakeDough deriving Show
data Cakedough = ChocolateDough | BananaDough | CherryDough deriving Show 
data Action = Bake Int deriving Show 
-- Функции, которые описывают процесс приготовления частей торта
makeCakeMix ::  Maybe Ingredients ->  Maybe Ingredients ->  Maybe Fillingmix
makeCakeMix  (Just (Oil n)) (Just (Chocolate m )) 
            | n>100 && n<150 && m > 200 && 250>m= Just Oilchocolatemix
makeCakeMix  (Just (Chocolate n)) (Just (Oil m )) 
            | m>100 && m<150 && n > 200 && 250>n= Just Oilchocolatemix

makeCakeMix  (Just (Banana n)) (Just (Oil m )) 
            | n>2 && n<5 && m > 100 && 150>m= Just Bananamix
makeCakeMix  (Just (Oil n)) (Just (Banana m )) 
            | n>100 && n<150 && m > 2 && 5>m= Just Bananamix    

makeCakeMix  (Just (Cherry n)) (Just (Sugar m )) 
            | n>5 && n<20 && m > 100 && 150>m= Just Cherrymix
makeCakeMix  (Just (Sugar n)) (Just (Cherry m )) 
            | n>100 && n<150 && m > 5 && 20>m= Just Cherrymix
makeCakeMix _ _ = Nothing 
                                 

cakeDough :: Maybe Ingredients -> Maybe Ingredients -> Maybe Ingredients -> Maybe Ingredients -> Maybe Dough
cakeDough (Just (Egg n)) (Just (Flour m)) (Just (Sugar s)) (Just (Bakingpowder b))
    | n > 2 && n < 4 && m > 100 && m < 150 && s > 200 && s < 250 && b >50 && b <75 = Just CakeDough


cakeDough _ _ _ _ = Nothing 

cakeDoughFinal :: Maybe Dough -> Maybe Fillingmix -> Maybe Cakedough 
cakeDoughFinal (Just CakeDough) (Just Oilchocolatemix) = Just ChocolateDough
cakeDoughFinal (Just CakeDough)(Just Cherrymix) = Just CherryDough
cakeDoughFinal (Just CakeDough) (Just Bananamix) = Just BananaDough
cakeDoughFinal _ _ = Nothing 

makeCake :: Maybe Cakedough -> Maybe Action -> Maybe Cake 
makeCake (Just ChocolateDough) (Just (Bake t)) 
    |t > 15 && t < 20 = Just ChocolateCake 
makeCake (Just BananaDough) (Just (Bake t)) 
    |t > 15 && t < 20 = Just BananaCake
makeCake (Just CherryDough) (Just (Bake t)) 
    |t > 15 && t < 20 = Just CherryCake
makeCake _ _ = Nothing    


testmix = makeCakeMix  (Just (Sugar 101)) (Just (Cherry 10))
testdough = cakeDough (Just (Egg 3))(Just (Flour 101))(Just (Sugar 201))(Just (Bakingpowder 51))
testfinaldough = cakeDoughFinal (Just CakeDough)(Just Cherrymix) 

testmake = makeCake(Just CherryDough) (Just (Bake 16))