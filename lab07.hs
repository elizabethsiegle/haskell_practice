{-# LANGUAGE TypeOperators, TypeInType, GADTs, TypeFamilies,
             UndecidableInstances, AllowAmbiguousTypes, ScopedTypeVariables,
             TypeApplications, PolyKinds, RankNTypes, DataKinds, FlexibleContexts #-}
module Class where
import Data.Type.Equality
import Data.Kind ( Type )
import System.IO
--Write a Haskell program that loops, asking the user for a number and then reporting the number’s 
--prime factors. Exit when the user enters in something other than a positive number.

--Implement the following utilities in Haskell:

--cat
--echo
--ls, with its -F option
--cp
--(Challenge) Ponder how to take your solution to (1) and add fancy types to it. 
--Note that prime factorization has a simple specification: it takes a number and 
--produces a list of numbers, each of which is prime and the product of which equals the original number. 
--If you have your algorithm run over SNats, you should be able to create a custom list-like GADT for your 
--output that captures these properties. You can then use such a structure to verify your algorithm.

prime_factors_help n =
  case factors of
    [] -> [n]
    _  -> factors ++ prime_factors_help (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]

getNum:: Int -> [Int]
getNum input
  | input < 0                        = error "no negatives"
  | (prime_factors_help input) == [] = error "no prime factors"
  | otherwise                        = prime_factors_help input

loop :: IO()
loop = do
  putStrLn "Enter a number to get the prime factorization"
  inp :: Int <- readLn
  print $ getNum inp
  loop

main = do
  loop