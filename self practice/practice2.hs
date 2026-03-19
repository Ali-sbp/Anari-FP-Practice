module Practice2 where
import System.Posix (accessModes)
--import Pr01_2 (MyList(MyEmpty))
--import Lec03 (myLength)

-- ============================================================
-- PRACTICE SESSION: foldr / foldl
-- Work through the warm-up tasks, then re-implement the tasks
-- from pr01_2.hs on your own.
-- ============================================================

--Напишите реализацию функций:
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

myLength :: [a] -> Int 
myLength list = foldr  (\x acc -> acc +1) 0 list 

myFilter :: [Int] -> [Int]
myFilter list = foldr (\x acc -> if even x then x : acc  else acc ) [] list

mySum2 :: [Int] -> Int
mySum2 list = foldl (\acc x -> acc + x) 0 list 

myreverse :: [a] -> [a]
myreverse (x:xs) = foldl (\acc x -> x : acc ) [] (x:xs)

myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p list = foldr (\x acc -> if (p x) then x : acc else []) [] list

myTakeWhile2 :: (a -> Bool) -> [a] -> [a]
myTakeWhile2 p list = foldr localF [] list  
    where 
        localF x acc 
            | p x = x : acc 
            | otherwise = [] 

mySpan1 :: (a -> Bool) -> [a] -> ([a], [a])
mySpan1 p list = foldr (\x (first, second) -> if p x then (x : first, second) else ([] , x: first ++ second)) ([], []) list 

mySpan2 :: (a -> Bool) -> [a] -> ([a], [a])
mySpan2 p list = foldr localF ([], []) list 
    where 
        localF x (left, right) 
            | p x = (x : left, right) 
            |otherwise = ([] , x: left ++ right) 


myfoldl1 :: (a->a->a) -> [a] -> Maybe a
myfoldl1 f [] = Nothing
myfoldl1 f (x:xs) = Just (foldl f x xs)


myfoldr1 :: (a-> a-> a) -> [a] -> Maybe a
myfoldr1 f [] = Nothing
myfoldr1 f [x] = Just x 
myfoldr1 f (x:xs) = 
    let Just rest = myfoldr1 f xs 
    in Just (f x rest)

result = let x = 5 
             y = 10 
             in x + y; 
result2 = let doubled = 10 *2 
              trippled = 10 *3 
              in doubled + trippled 

myfoldr2 :: (a-> a -> a) -> [a] -> Maybe a
myfoldr2 f [] = Nothing 
myfoldr2 f [x] = Just x 
myfoldr2 f (x:xs) = 
    let Just val = myfoldr2 f xs 
    in Just (f x val)


doubleIfExists :: Maybe Int -> Maybe Int 
doubleIfExists (Just input) = (Just result) 
    where
        Just result = Just (input * 2)
doubleIfExists Nothing = Nothing 

doubleIfExists1 :: Maybe Int -> Maybe Int 
doubleIfExists1 (Just input) = 
    let  result = input * 2
    in Just result 
doubleIfExists1 Nothing = Nothing 


doubleIfExists2 :: Maybe Int -> Maybe Int 
doubleIfExists2 input = case input of 
    Just input -> Just (input * 2)
    Nothing -> Nothing 

addTen :: Maybe Int -> Maybe Int 
addTen (Just input) = Just (input + 10)
addTen Nothing = Nothing 

addTen1 :: Maybe Int -> Maybe Int 
addTen1 mx = case mx of 
    Just asd -> Just (asd + 10)
    Nothing -> Nothing 

addTen2 :: Maybe Int -> Maybe Int 
addTen2 (Just input) = Just result 
    where 
        result = input + 10 
addTen3 :: Maybe Int -> Maybe Int
addTen3 (Just input) = 
    let  result = (input + 10)
    in Just result

reverseString :: Maybe String -> Maybe String 
reverseString Nothing = Nothing 
reverseString (Just str) = 
    let res = myreverse str 
    in Just res 

reverseString2 :: Maybe String -> Maybe String 
reverseString2 mx = case mx of 
    Just asd -> Just (myreverse asd)
    Nothing -> Nothing 

reverseString3 :: Maybe String -> Maybe String 
reverseString3 (Just asd) = Just res 
    where 
        res = myreverse asd 
reverseString3 Nothing = Nothing 

ispositive :: Maybe Int -> Maybe Bool 
ispositive (Just asd) 
    | asd > 0 = Just True 
    | otherwise = Just False
ispositive Nothing = Nothing 

--unsure about syntax
ispositive1 :: Maybe Int -> Maybe Bool 
ispositive1 mx = case mx of 
    Just asd | asd > 0 -> Just True
             |otherwise -> Just False 
    Nothing -> Nothing

ispositive2 :: Maybe Int -> Maybe Bool 
ispositive2 (Just asd) = res 
    where 
        res |asd > 0 = Just True 
            |otherwise = Just False 
ispositive2 Nothing = Nothing 

ispositive3 :: Maybe Int -> Maybe Bool 
ispositive3 (Just asd) = 
    let res |asd > 0  = Just True
            |otherwise = Just False 
    in  res 
ispositive3 Nothing = Nothing 


myMaybe :: b -> (a -> b) -> Maybe a -> b 
myMaybe defval f Nothing = defval 
myMaybe defval f (Just x) = 
    let res = f x
    in res 

myMaybe1 :: b -> (a -> b) -> Maybe a -> b 
myMaybe1 defv f mx = case mx of 
    Nothing -> defv 
    Just x -> f x 

myMaybe2 :: b -> (a -> b) -> Maybe a -> b 
myMaybe2 defv f Nothing = defv 
myMaybe2 defv f (Just x) = 
    let res = f x 
    in res 
myMaybe3 :: b -> ( a-> b) -> Maybe a -> b 
myMaybe3 defv f Nothing = defv 
myMaybe3 defv f (Just x) = res 
    where 
        res = f x 

data MyList a = MyEmpty | MyCons a (MyList a)

myMap :: (a->b) -> MyList a -> MyList b
myMap f MyEmpty = MyEmpty
myMap f (MyCons x xs ) = MyCons (f x) (myMap f xs)

defMap :: (a->b) -> [a] -> [b]
defMap f [] = [] 
defMap f (x:xs) = (f x) : defMap f xs 

myUnfoldr1 :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr1 f seed = case f seed of 
    Nothing -> []
    Just (x, nextseed) -> myUnfoldr1 f nextseed

myUnfoldr2 :: (b-> Maybe (a,b)) -> b -> [a]
myUnfoldr2 f seed = asd (f seed)
    where 
        asd Nothing = []
        asd (Just (x , nextseed))= myUnfoldr2 f nextseed

myUnfoldr3 :: (b -> Maybe(a, b)) -> b -> [a]
myUnfoldr3 f seed = myMaybe [] (\(x , nextseed) -> x : myUnfoldr3 f nextseed ) (f seed)


myUnfoldr4 :: (b -> Maybe(a, b)) -> b -> [a]
myUnfoldr4 f seed  
    | f seed == Nothing = []
    | otherwise = []

--funC :: Either [a -> b ] [c] (a -> b) Either b c  