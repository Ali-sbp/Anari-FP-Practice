-------------------------------------
-- Практические задание 1. Часть 1 --
-------------------------------------

module Pr01_1 where

{-

Напишите реализацию функций myFST, mySND, myTHRD для кортежа (a,b,c)



Напишите реализацию стандартных функции для работы со списками:
-- myHead - определение (через сопоставление с образцом) функции отделения головы списка
-- myTail - функция отделения хвоста списка
-- myTake - взять первые n элементов списка
-- myDrop - отбросить первые n элементов списка
-- myProduct - перемножить все элементы списка
-- myZip - попарное объединение двух списков в список пар, длина итогового списка по длине самого короткого из входных списков
-- myZip3 объединение трех списков в список троек
-- myUnzip - разделение списка пар на пару списков

Напишите реализацию стандартных функции высшего порядка для работы со списками:
-- myFilter - применение предиката к каждому элементу списка (две реализации: с использованием охранных выражений и if-then-else)
-- myMap - применение функции одного аргумента к каждому элементу списка
-- myZipWith - применение функции двух аргументов к двум спискам
-- myZipWith3 - применение функции трех аргументов к трем спискам
-- myAll - проверяет удовлетворяют ли все элементы списка предикату
-- myAny - проверяет удовлетворяют ли хотя бы один элемент списка предикату
-- myComposition - композиция двух функций (.)

-}
import Prelude --for zip

-- myFST, mySND, myTHRD : 

myFST :: (a , b , c ) -> a 
myFST (x , y , z) = x 

mySND :: (a , b , c) -> b 
mySND (x , y , z) = y 

myTHRD :: (a, b , c) -> c 
myTHRD (x , y , z) = z 

--myHead 

myHead :: [a] -> a 
myHead (x : xs) = x 
myHead [] = error " empty list" 

{-
myHead :: [a] -> a 
myHead (x : _) = x  : Q1: this also works right?
myHead [] = error " empty list" 
-}
--myTail
myTail :: [a] -> [a]
myTail (x : xs) = xs  -- myTail (_ : xs) = xs 
myTail []  = error " empty list"

--myTake
myTake :: Int -> [a] -> [a]
myTake 0 _ = [] -- 0 case
myTake _ [] = [] -- empty list case
myTake n (x : xs) = x : myTake (n - 1) xs -- calling it with n-1 on the tail of the first list recursively

--myDrop

myDrop :: Int -> [a] -> [a]
myDrop 0 (x : xs)= x : xs 
myDrop _ [] = []
myDrop n (x : xs) = myDrop (n - 1) xs 

--myProduct
myProduct :: [Int] -> Int 
myProduct [] = 1
myProduct (x : xs) = x *(myProduct xs) -- myProduct (x : xs) = foldr (*) 1 xs extension suggested this 


--myZip
myZip :: [a] -> [b] -> [(a,b)]
myZip [] xs = []
myZip xs [] = []
myZip (x:xs) (y:ys) = (x,y) : myZip xs ys 

--myZip3
myZip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
myZip3 [] _ _ = [] 
myZip3 _ [] _ = []
myZip3 _ _ [] = []
myZip3 (x:xs) (y:ys) (z:zs) = (x,y,z) : myZip3 xs ys zs  

--myUnzip

myUnzip :: [(a,b)] -> ([a] , [b]) 
myUnzip [] = ([] , [])
myUnzip((a , b) : rest) =
    let (as , bs) = myUnzip rest
    in (a : as, b: bs)
        
-- second part : 



myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x :xs) = f x : myMap f xs


myzipWith :: (a-> b -> c) -> [a] -> [b] -> [c]
myzipWith f [] xs = [] -- myzipWith _ [] _ = []
myzipWith f xs [] = [] -- myzipWith _ _ [] = []
myzipWith f (x : xs) (y:ys) = f x y : myzipWith f xs ys 

