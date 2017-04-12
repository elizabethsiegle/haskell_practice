-- # LANGUAGE FlexibleContexts #
import Data.Char

square' x = x * x
showResult:: Int -> String
showResult y = "The result is " ++ show y

showAreaOfCircle:: Double -> String
showAreaOfCircle z = "The area of is " ++ show (pi*(z**2)) 

sort2:: Ord a => a -> a -> (a, a)
--guard??
sort2 a b | a > b     = (a, b)
          | b < a     = (b, a)
          | otherwise = (a, a)
--cond
sort2Cond :: Ord a => a -> a -> (a, a)
sort2Cond a b = if b > a 
    then (b, a)
    else if b < a 
        then (a, b)
    else (a, a)

almostEqual:: Eq a => (a, a) -> (a, a) -> Bool
almostEqual (x1, y1) (x2, y2)
    | (x1 == x2) = (y1 == y2) 
    | (x1 == y2) = (y1 == x2) 
    | otherwise  = False

isLower :: Char -> Bool
isLower c | c `elem` ['a'..'z']    = True
          | otherwise              = False

charToString :: Char -> String
charToString c = [c]
mangle:: String -> String
mangle str 
    | length str == 0 = ""
    | length str == 1 = str
    | otherwise       = tail str ++ (charToString (head str)) --(show (tail str) : (init str))

retMult :: Int -> [Int]
retMult m = [m*1..m*12]

div1 :: Int -> Int -> Int
div1 d1 d2
    | d2 `elem` retMult d1    = d2 `div` d1
    | otherwise               = d2

natSum:: (Num a, Ord a) => a -> a
natSum 0              = 0
natSum n | n > 0      = n + natSum(n-1)
         | otherwise  = error "natSum: input val too small"

repeatN:: Int -> Int -> [Int]
repeatN 0 x = []
repeatN n x = x : repeatN (n-1) x

--first char followed by calling func on tail
repeatStrIt:: String -> [String]
repeatStrIt a | a == ""    = []
              | otherwise  = a : repeatStrIt (tail a) 

--square the head then call func on tail
allSquares :: Num a => [a] -> [a]
allSquares []      = []
allSquares (x: xs) = x*x : allSquares xs

allToUpper:: String -> String
allToUpper []      = []
allToUpper (s: tr) = toUpper s : allToUpper tr

extractDigits:: String -> String
extractDigits []    = []
extractDigits (s : tr) 
    | isDigit s     = s : extractDigits tr --is first char dig if so include
    | otherwise     = extractDigits tr --1st char !digit, don't include

prod :: Num a => [a] -> a
prod []     = 1
prod (x:xs) = x * prod xs

sum2 :: Num a => [a] -> a
sum2 [] = 0
sum2 (hmm : huh) = hmm + sum2 huh

rev :: [a] -> [a]
rev [] = []
rev (lol : lol2) = reverse lol2 ++ [lol]

deductFromAcc:: Int -> [Int] -> Int
deductFromAcc balance [] = balance
deductFromAcc balance (d : ds) 
    | balance < d = error("Balance = " ++ show balance ++ "Can't deduct " ++ show d)
    | otherwise   = deductFromAcc (balance - d) ds --balance = accumulator

str2Int:: String -> Int
str2Int str = str2IntAcc 0 str

str2IntAcc :: Int -> String -> Int
str2IntAcc a [] = a
str2IntAcc a (c : r) 
    = str2IntAcc (10*a + digitToInt c) r 

sumEvenElems :: Num a => [a] -> a
sumEvenElems ex = sum (filtEven ex)
  where  
  	filtEven []  
  	  = []
  	filtlEven (x : ex)
  	 | even x    = x : filtEven ex
  	 | otherwise = filtEven ex



--lists HO from Prof. Eisenberg 1/30/17
sumMult::Integer
sumMult = sum [x | x <- [0, 1..1000], x `mod` 3 == 0 ||  x `mod` 5 == 0]

fibs::[Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
helperFibs = takeWhile (<4000000) fibs
sumOfEvenFibsLT4m = sum[x | x <- helperFibs, x `mod` 2 == 0  && x < 4000000]
--sumOfEvenFibsLT4m = sum $ filter even $ takeWhile (\x -> x < 4000000) fibs

--nog::Bool->Bool
--nog x = [x]
--idBool::Bool->Bool
--idBool y = [y*y]