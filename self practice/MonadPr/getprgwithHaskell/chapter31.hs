--chapter 31
module Asd where  
import qualified Data.Map as Map 
import Pizza 
askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = "Hello, " ++ name ++ "!"

-- helloName :: IO ()
-- helloName = askForName >>
--             getLine >>=
--             (\name -> 
--                 return (nameStatement name)) >>=
--             putStrLn    

maxPairM :: (Monad m, Ord a) => m (a,a) -> m a
maxPairM  maa = maa >>= (\(x,y) -> return (max x y))

readInt :: IO Int 
readInt = read <$> getLine
-- Write a program by using the tools of the Monad type class that takes a
-- pair of values in a context, and then returns the maximum of each pair. Here’s your type
-- signature to get you started:
-- maxPairM :: (Monad m, Ord a) => m (a,a) -> m a
-- The resulting function should work on IO (a,a), Maybe (a,a), and [(a,a)].


-- main :: IO () 
-- main = do 
--     putStrLn "input a"
--     first <- readInt
--     putStrLn "input b"
--     second <- readInt 
--     result <- maxPairM $ return ( (,) first second ) 
--     putStrLn $ "here is the max " ++ show result


-- main :: IO () 
-- main = do 
helloName :: IO () 
helloName = do 
    --putStrLn " whats ur name?"
    askForName
    name <- getLine
    let statement = nameStatement name 
    putStrLn statement

echo :: IO () 
echo = do 
    str <- getLine
    putStrLn str 

--candidate interview problem : 

data Grade = F | D | C | B | A deriving (Eq, Ord, Enum, Show, Read)

data Degree = HS | BA | MS | PhD deriving (Eq, Ord, Enum, Show, Read)

data Candidate = Candidate
    { candidateId :: Int
    , codeReview :: Grade
    , cultureFit :: Grade
    , education :: Degree } deriving Show

viable :: Candidate -> Bool
viable candidate = all (== True) tests where 
            passedCoding = codeReview candidate > B
            passedCultureFit = cultureFit candidate > C
            educationMin = education candidate >= MS
            tests = [passedCoding
                    ,passedCultureFit
                    ,educationMin]
ali = Candidate 1 A A PhD

readID :: IO Int 
readID = getLine >>= (return . read)

readGrade :: IO Grade 
readGrade = getLine >>= (return . read)

readGrade' :: IO Grade 
readGrade' = do 
    grd <- getLine
    let asd = read grd 
    return asd 

readDegree :: IO Degree
readDegree = getLine >>= (return . read) 

readCandidate :: IO Candidate
readCandidate = do 
    putStrLn " input candidate Id "
    cid <- readID 
    putStrLn "input candidate codereview"
    codeR <-readGrade 
    putStrLn "input candidate culturefit"
    cultfit <- readGrade
    putStrLn "input candidate degree"
    educ <- readDegree
    return (Candidate {
                       candidateId = cid ,
                       codeReview = codeR ,
                       cultureFit = cultfit ,
                       education = educ})
    
assessCandidate :: IO String 
assessCandidate = do 
    candidate <- readCandidate 
    let passed = viable candidate 
    let statement = if passed then "passed" else "failed"
    return statement 


                    
-- main :: IO () 
-- main = do 
--     assessCandidate >>= putStrLn 

candidate1 :: Candidate
candidate1 = Candidate { candidateId = 1
                        , codeReview = A
                        , cultureFit = A
                        , education = BA }
candidate2 :: Candidate
candidate2 = Candidate { candidateId = 2
                        , codeReview = C
                        , cultureFit = A
                        , education = PhD }
candidate3 :: Candidate
candidate3 = Candidate { candidateId = 3
                        , codeReview = A
                        , cultureFit = B
                        , education = MS }

candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList [(1,candidate1)
                            ,(2,candidate2)
                            ,(3,candidate3)]

assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe cId = do
    candidate <- Map.lookup cId candidateDB
    let passed = viable candidate
    let statement = if passed
        then "passed"
        else "failed"
    return statement

-- QC 31.4 
hmm :: Maybe String -> String 
hmm (Just s) = "failed/passed"
hmm _ = "error , not found"

-- ?????

candidates :: [Candidate]
candidates = [candidate1
            ,candidate2
            ,candidate3]

assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do 
    candidate <- candidates 
    let passed = viable candidate  
    let statements = if passed then "passed" else "failed"
    return statements  

-- this return thing is magic , i can make the function like : [candidate] -> Maybe String and the 
-- return will just wrap it back into Maybe type, 

-- assessCandidates :: [Candidate] -> [String]
-- assessCandidates candidates = map (\x -> if x
--                                         then "passed"
--                                         else "failed") passed
--     where passed = map viable candidates
-- for non monad thinking idiots <3 


-- jesus assessCandidate , holy fk monads are powerful!
assessCandidateM :: Monad m => m Candidate -> m String
assessCandidateM candidates = do
    candidate <- candidates 
    let passed = viable candidate 
    let statement = if passed then "passed" else "failed"
    return statement

-- Q31.1 At the end of lesson 21, you saw the following program used to calculate the
-- cost of pizza:
-- main :: IO ()
-- main = do
--     putStrLn "What is the size of pizza 1"
--     size1 <- getLine
--     putStrLn "What is the cost of pizza 1"
--     cost1 <- getLine
--     putStrLn "What is the size of pizza 2"
--     size2 <- getLine
--     putStrLn "What is the cost of pizza 2"
--     cost2 <- getLine
--     let pizza1 = (read size1, read cost1)
--     let pizza2 = (read size2, read cost2)
--     let betterPizza = comparePizzas pizza1 pizza2
--     putStrLn (describePizza betterPizza)
-- Q31.1 Desugar this code to use >>=, >>, return and lambda functions rather than do-notation.
main :: IO ()
main =
    putStrLn "What is the size of pizza 1" >>
    getLine >>= (\size1 ->
    putStrLn "What is the cost of pizza 1" >>
    getLine >>= (\cost1 ->
    putStrLn "What is the size of pizza 2" >>
    getLine >>= (\size2 ->
    putStrLn "What is the cost of pizza 2" >>
    getLine >>= (\cost2 ->
        let pizza1      = (read size1, read cost1)
            pizza2      = (read size2, read cost2)
            betterPizza = comparePizzas pizza1 pizza2
        in putStrLn (describePizza betterPizza)))))

-- AHAHAHAHAHAHAH :
-- Lesson 31
-- Q31.1 Now that you’ve done this once, you’ll never again forget how useful do-nota-
-- tion is! KURT YOU ARE CRAZY AHAHHAHAHAH 

-- Q31.2 At the end of lesson 21 in unit 4, we first introduced the idea that do-notation
-- isn’t specific to IO. You ended up with this function for a Maybe type:
-- maybeMain :: Maybe String
-- maybeMain = do
--     size1 <- Map.lookup 1 sizeData
--     cost1 <- Map.lookup 1 costData
--     size2 <- Map.lookup 2 sizeData
--     cost2 <- Map.lookup 2 costData
--     let pizza1 = (size1,cost1)
--     let pizza2 = (size2,cost2)
--     let betterPizza = comparePizzas pizza1 pizza2
--     return (describePizza betterPizza)
-- -- Rewrite this function so it works with the List type (don’t worry if the results seem
-- -- strange).
-- maybeMainList :: [String]
-- maybeMainList = do
--     size1 <- sizesData -- sizesData = [10,11,12,14 ...]
--     cost1 <- costsData -- costsData = [40,45,50 ...]
--     size2 <- sizesData
--     cost2 <- costsData
--     let pizza1 = (size1,cost1)
--     let pizza2 = (size2,cost2)
--     let betterPizza = comparePizzas pizza1 pizza2
--     return (describePizza betterPizza)

-- Q31.3 Refactor the maybeMain function from the preceding exercise so that it works with
-- any Monad. You’ll need to change the type signature as well as remove the type-specific
-- parts from the body of the function.

monadMain :: Monad m => m Sizes -> m Costs -> m String 
monadMain sizes costs  = do
    size1 <- sizes -- some size container , [] , Map.lookup 1 sizeDB or ...
    cost1 <- costs -- some cost container , [] , Map.lookup 1 sizeDB or ...
    size2 <- sizes 
    cost2 <- costs 
    let pizza1 = (size1,cost1)
    let pizza2 = (size2,cost2)
    let betterPizza = comparePizzas pizza1 pizza2
    return (describePizza betterPizza)