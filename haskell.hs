-- Comments are like this
{-
Multiline comments like this
-}

-- Lets get started
-- all commands that need to be run are enclosed in backticks ( ` )
-- open terminal
-- goto the folder containing this file
-- `ghci`
-- `:l haskell`
-- `:r`

-- import modules like this
-- List module
import Data.List
-- IO module
import System.IO

-- Haskell is statically type , i.e, you cant change the type once you define a constant
-- Most commonly used datatypes are :
-- - Int
-- - Integer - Large Int, Float , Double
-- - Bool - True / False
-- - Char - 'x'
-- - String - "string"
-- - Lists - [1, 2]
-- - Tuple - (1, '2')

-- VARIABLES
--------------

-- can manually declare type
always5 :: Int
-- immutable values
-- value cant be changed once defined , i.e, all variables are constants
always5 = 5
-- `always5` to see value of variable
-- or can leave type inference to haskell
addEx = 5 + 5

-- some operators are prefixed
modEx = mod 5 4
-- but can be infixed with `
modEx2 = 5 `mod` 4

-- negative numbers should be wrapped in ()
negNumEx = 5 + (-4)

-- `:t sqrt` to get function declaration
-- sqrt :: Floating a => a -> a
-- Means the type of argument is floating and it takes 1 argument and returns a floating number

num9 = 9 :: Int
-- fromIntegral function to convert to Float as per definitition -  since num9 is Int
sqrtOf9 = sqrt (fromIntegral num9)

-- built in math functions
piVal = pi
ePow9 = exp 9
logOf9 = log 9
squared9 = 9 ** 2
-- truncate round ciel floor

-- :t truncate
-- truncate :: (RealFrac a, Integral b) => a -> b , i.e, it takes a Real fraction (double,float) and returns an Integer
truncateVal = truncate 9.999

-- also sin, sinh ,cos ...

-- Bool
trueAndFalse = True && False
trueOrFalse = True || False
notTrue = not(True)

-- LISTS
-----------
-- same datatype
-- singly linked
-- only add to front

primeNumbers = [3, 5, 7, 11]

-- concatenate lists like this
morePrimes = primeNumbers ++ [13, 17, 19, 23, 29]
favNums = 2 : 3 : 5 : 10 : 11 :16 : []

morePrimes2 = 2 : morePrimes

-- Can run functions on these lists like this
revPrime = reverse morePrimes2
isListEmpty = null morePrimes2

-- use !! to get element at a particular position
secondPrime = morePrimes2 !! 1
firstPrime = head morePrimes2
lastPrime = last morePrimes2
allButLast = init morePrimes2

first3Primes = take 3 morePrimes2
removed3Primes = drop 3 morePrimes2
removeFLast3Primes = reverse (drop 3 (reverse morePrimes2))

is7InList = 7 `elem` morePrimes2
-- or prefix as -  is7InList = elem 7 morePrimes2

maxPrime = maximum morePrimes2
minPrime = minimum morePrimes2


multList = [[1,2,3],[4,5,6]]
sumOfNums = sum [1..1000]

zeroToTen = [1..10]
evenList = [2,4..20]

evenLetterList = ['A','C'..'Z']

-- Infinite lists - Lazy , ie, Calculates only when you ask for it
infinitePow10 = [10,20..]

-- repeat 2s and 3s inifinitely - take first 10 numbers
many2s = take 10 (repeat 2)
many3s = replicate 10 3

cycleLists = take 10 (cycle [1,2,3,4])

factorial n = product [1..n]
factorial5 = factorial 5

listTimes2 = [x * 2 | x <- [1..10]]
-- filters
listTimes3LessThan50 = [x * 3 | x <- [1..10], x * 3 <= 50]
divisibleBy9N13 = [x | x <- [1..500], x `mod` 13 == 0, x `mod` 9 == 0]

sortedList = sort [3,5,1,2,9,23,34,79,2,1,43,32]

sumOfLists = zipWith (+) [1,2,3,5,3,4,5] [4,2,3,5,7,2,4]

listBiggerThan5 = filter (>5) morePrimes
evensUpto20 = takeWhile (<=20) [2,4..]

-- fold left ie, 1*2*3*4*5, use foldr for right fold
multOfList = foldl (*) 1 [2,3,4,5]

pow3List = [3^n | n <- [1..10]]

multiplicationTable = [ [x * y | x <- [1..10]] | y <- [1..10]]

--tuples
--------
--can have multiple datatypes

randTuple = (1, "Random Tuple", 'H')

bobSmith = ("Bob Smith", 23)
bobsName = fst bobSmith
bobsAge = snd bobSmith

names = ["Bob", "Mary", "Tom"]
age = [23,41,34]
nameNAge = zip names age

-- functions
-------------
-- type declaration
-- funcName param1 param2 = ..operations -> (returned value)
addMe :: Int -> Int -> Int
addMe x y = x + y

-- or leave it tp type inference
sumMe x y = x + y

addTuples :: (Int, Int) -> (Int, Int) -> (Int, Int)
addTuples (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)


-- value base functions
whatAge :: Int -> String
whatAge 18 = "Can vote"
whatAge 21 = ""
whatAge 24 = "Adult"
whatAge _ = "Nothing much"


recursiveFactorial :: Int -> Int
recursiveFactorial 0 = 1
recursiveFactorial n = n * recursiveFactorial(n - 1)

-- gaurd for list of qualifiers
isOdd :: Int -> Bool
isOdd n 
  | n `mod` 2 == 0 = False
  | otherwise = True

isEven n = n `mod` 2 == 0 

-- where clause
battingAverage :: Double -> Double -> String
battingAverage matches runs
  | avg < 35 = "Bad"
  | avg < 50 = "OK"
  | avg < 60 = "Good"
  | otherwise = "WOAH!"
  where avg = runs / matches

getListItems :: [Int] -> String

getListItems [] = "Your list is empty"
-- use show to convert things to string
getListItems (x:y:[]) = "Your list starts with " ++ show x ++ " and" ++ show y
--getListItems (x:xs) = "Your list starts with " ++ show x ++ " and ends with" ++ show xs

getFirstItem :: String -> String
-- string can be treated as lists
getFirstItem [] = "Empty string"
getFirstItem all@(x:xs) = "The first letter in " ++ all ++ " is " ++ show x ++ "and the rest is " ++ show xs

recursiveListTimes4 :: [Int] -> [Int]
recursiveListTimes4 [] = []
recursiveListTimes4 (x:xs) = times4 x : recursiveListTimes4 xs

areStringsEqual :: [Char] -> [Char] -> Bool
areStringsEqual [] [] = True
areStringsEqual (x:xs) (y:ys) = x == y && areStringsEqual xs ys
areStringsEqual _ _ = False

-- higher order functions
-- functions as first order citizens that can be passed around

times4 :: Int -> Int
times4 x = x * 4

listTimes4 = map times4 [1, 2, 3, 4, 5]


callWith3 :: (Int -> Int) -> Int
callWith3 func = func 3 

num3Times4 = callWith3 times4

-- _.partial
getAddFunc :: Int -> (Int -> Int)
getAddFunc x y = x + y

add3 = getAddFunc 3

fourPlus3 = add3 4

thrrePlusList = map add3 [1,2,3,4,5]

-- lambda - Anonymous function
dbl1to10 = map (\x -> x * 2) [1..10]

-- ifs
doubleEvenNumbers y =
  if (y `mod` 2 /= 0)
    then y
    else y * 2 

-- case
getClass :: Int -> String
getClass n = case n of
  5 -> "Kindergarden"
  6 -> "school"
  _ -> "zz"

-- Enums
data CricketPlayer = Batsman
                    | Bowler
                    | Fielder
                    | Keeper
                  deriving Show

isBatsman :: CricketPlayer -> Bool
isBatsman Batsman = True
isBatsman _ = False

-- Custom data types
data Customer = Customer String String Double
  deriving Show

tom :: Customer
tom = Customer "Tom" "123 main" 100.5

getBalance :: Customer -> Double
getBalance (Customer _ _ b) = b  

tomsBalance = getBalance tom

-- Rock paper scissor
data RPS = Rock | Paper | Scissor

shoot :: RPS -> RPS -> String
shoot Paper Rock = "Paper beats rocks"
shoot Rock Scissor = "Rock beats scissors"
shoot Scissor Paper = "Scissor beats paper"
shoot Scissor Rock = "Scissor Loses to Rock"
shoot Paper Scissor = "Paper Loses to scissors"
shoot Rock Paper = "Rock loses to paper"
shoot _ _ = "Error"

data Shape = Circle Float Float Float | Rectangle Float Float Float Float

area :: Shape -> Float
area (Circle _ _ r) = pi * (r ^ 2)
-- can use $ to get rid of parenthesis - evaluates immediate expression
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs (y2 - y1))

areaOfCircle = area (Circle 50 60 20)
areaOfRect = area $ Rectangle 10 20 30 40

-- can use dot operator to chain functions - to left from input on right
sumVal = putStrLn ( show (1 + 2))
sumVal2 = putStrLn . show $ 1 + 2

-- Typed classes

data Employee = Employee { name :: String, position :: String, idNum :: Int }
  deriving (Eq, Show)

mukesh = Employee {name = "Mukesh", position = "Engineer", idNum = 550}
nitesh = Employee {name = "Nitesh", position = "Engineer", idNum = 420}

isMukeshNitesh = mukesh == nitesh

mukeshData = show mukesh

-- can override deriving
data ShirtSize = S | M | L

instance  Eq ShirtSize where
  S == S = True
  M == M = True
  L == L = True
  _ == _ = False

instance Show ShirtSize where
  show S = "small"
  show M = "medium"
  show L = "large"

smallAvail = S `elem` [S, M, L]

smallSizeAsString = show S

-- Custom class to check equality
class MyEq a where
  areEqual :: a -> a -> Bool

instance MyEq ShirtSize where
  areEqual S S = True
  areEqual M M = True
  areEqual L L = True
  areEqual _ _ = False


mNmEqual = areEqual M M

-- IO

sayHello = do
  putStrLn "Whats you name"
  name <- getLine
  putStrLn $ "Hello " ++ name


writeToFile = do
  theFile <- openFile "test.txt" WriteMode
  hPutStrLn theFile ("Random line of text")
  hClose theFile

readFromFile = do
  theFile <- openFile "test.txt" ReadMode
  contents <- hGetContents theFile
  putStrLn contents
  hClose theFile

-- recursive fibbonacci series
fib = 1 : 1 : [a + b | (a, b) <- zip fib (tail fib)]








