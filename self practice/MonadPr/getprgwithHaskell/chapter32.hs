import Control.Monad (guard)
import Data.Char (toUpper)
a = [x^2 | x <- [1..19], odd x]

powsOfTwo :: Int -> [Int]
-- powsOfTwo n = do 
    -- val <- [1..n]
    -- return (2^val)

powsOfTwo n = [2^val | val <- [1..n]]


pows2and3 :: Int -> [(Int,Int)]
-- pows2and3 n = do 
--     val <- [1..n]
--     let powsof2= 2^val 
--     let powsof3= 3^val 
--     return (powsof2, powsof3)

pows2and3 n = [(2^val,3^val) | val <- [1..n] ]

allevenOds :: Int -> [(Int,Int)]
-- allevenOds n = do 
--     evenvals <- [2,4 .. n]
--     oddvals <- [1,3 .. n]
--     return (evenvals, oddvals)

allevenOds n = [(evenvals,oddvals) | evenvals <- [2,4..n] , oddvals <- [1,3..n]]

numandSq::[(Int,Int)]
-- numandSq = do 
--     nums <- [1..10]
--     let sqs = nums^2 
--     return (nums, sqs)

numandSq = [(val,val^2) | val <- [1..10]]


-- TO ASk, guard type 
-- GHC.Base.Alternative f => Bool -> f () , what is f () 
genEvensGuard :: Int -> [Int]
genEvensGuard n = do 
    vals <- [1..n]
    guard(even vals)
    return vals 
-- can use guard on containers that have "empty" (are an instance of Alternative). 
-- for lists it's [] , for Maybe its Nothing
-- but IO doesn't have that, so you can't use guard with IO type!

genEvensGuard' :: Int -> [Int]
genEvensGuard' n = [val | val<- [1..n], even val] -- notice how guard is kinda removed in this notation ***


guardFilter :: (a -> Bool) -> [a] -> [a]
guardFilter pred vals = do 
    val <- vals 
    guard (pred val)
    return val 

guardFilter' :: (a->Bool) -> [a] -> [a]
guardFilter' pred vals = [val | val <- vals, pred val]

--even squars up to n
evenSqs :: Int -> [Int]
evenSqs n = do 
    val <- [1..n]
    -- let nSq = val^2 and ... (check the squared with guard not the val) but both work cause even * even = even
    guard (even val)
    return (val^2)

evenSqs' :: Int -> [Int]
evenSqs' n = [valSqed | val<-[1..n] , let valSqed=val^2, even valSqed]

-- Quick check 32.3 Write a list comprehension that takes the following words
-- ["brown","blue","pink","orange"]
-- and capitalizes the first letter, and prepends Mr. in front. (Hint: use Data.Char’s toUpper.)
fruits = ["apple","pear","kiwi","orange"]
mrFruit :: [String] -> [String]
mrFruit inputlist = do 
    fruit <- inputlist 
    
    let uppderCasedFruit = toUpper (head fruit) : tail fruit
    let appendedUpperCasedFruit = "Mr." ++ uppderCasedFruit 
    return appendedUpperCasedFruit 

mrFruit' :: [String] -> [String]
mrFruit' inputlist = [appendedUpperCasedF | fruit <- inputlist, 
                                            let uppercasedF = toUpper (head fruit) : tail fruit, 
                                            let appendedUpperCasedF="Mr." ++ uppercasedF]

-- Q32.1 Use a list comprehension that generates a list of correct calendar dates, given
-- that you know the number of days in each month. For example, it should start with 1 ..
-- 31 for January and be followed by 1 .. 28 for February.

-- monthesOfY = ["Jan", "Feb", "Mar", "Apr", "May", "Jun","Jul","Aug","Sep","Oct","Nov","Dec"]
-- data Month = Jan Int | Feb Int | Mar Int | Apr Int | May Int | Jun Int | 
--              Jul Int | Aug Int | Sep Int | Oct Int | Nov Int | Dec Int deriving (Show,Ord,Eq)
-- normalYear = [Jan 31, Feb 28, Mar 31, Apr 30, May 31, Jun 30, Jul 31, Aug 31, Sep 30, Oct 31, Nov 30, Dec 31]
-- daysIn :: Month -> Int
-- daysIn (Jan n) = n
-- daysIn (Feb n) = n
-- daysIn (Mar n) = n
-- daysIn (Apr n) = n
-- daysIn (May n) = n
-- daysIn (Jun n) = n
-- daysIn (Jul n) = n
-- daysIn (Aug n) = n
-- daysIn (Sep n) = n
-- daysIn (Oct n) = n
-- daysIn (Nov n) = n
-- daysIn (Dec n) = n
-- correctCalDates:: [(Month, Int)]
-- correctCalDates = [(month,day) | month <- normalYear, day <- [daysIn month] ]

correctCalDates :: [(Int,[Int])]
correctCalDates = [(month, days) | (month,days) <- zip [1..12] (map (\n -> [1..n])[31,28,31,30,31,30,31,31,30,31,30,31])
                                                                 ]

corrCalDates :: [(Int,[Int])]
corrCalDates = do
    (month,days) <- zip [1..12] (map (\n -> [1..n]) [31,28,31,30,31,30,31,31,30,31,30,31])
    return (month,days)

demon:: [(Int,[Int])]
demon = zip [1..12] (map (\n -> [1..n]) [31,28,31,30,31,30,31,31,30,31,30,31]) >>= \(month,days) ->
    return (month,days)

-- Q32.2 Translate the preceding question into do-notation, and then into Monad methods
-- and lambdas.