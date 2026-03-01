-------------------------------------
-- Практические задание 1. Часть 2 --
-------------------------------------

module Pr01_2 where
import GHC.Base (BCO, DoubleBox)

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
myFoldl1 :: (a->a->a) -> [a] -> Maybe a 
myFoldl1 f [] = Nothing 
myFoldl1 f (x:xs) = Just (myFoldl f x xs)

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