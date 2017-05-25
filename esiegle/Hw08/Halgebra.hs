{- CS380 Assignment 8
   Name:
   College email:
   Resources / collaborators:
-}

{-# LANGUAGE GADTs, StandaloneDeriving, TypeInType, TypeOperators #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Halgebra where

import Arith
import LArith
import Parser
import Data.Ratio
import Data.Kind ( Type )


--------------------------------------------------------------------------------
-- The Halgebra computer algebra system
--

{-
   See commentary with Assignment 3 for an oveview.

   With this new, GADT-ified version, we have an extra step:

   0. Convert the Equation into an LEquation. This conversion is,
      of course, partial: the original Equation might not be linear.
      The tricky part is in multiplication: before converting
      the factors in a multiplication, you must check which factor
      (if any) mentions x, and convert accordingly.

   Note that, in this version, extractResult returns a Maybe type,
   allowing a program to report a sensible error instead of just terminating
   when solving a bad equation. With this change, you should be able
   to solve an equation with no need for `error`.
-}
--test
sumx :: Sum
sumx = (parseSum "2 * x")

sum2 :: Sum
sum2 = (parseSum "2 + 2")
sum1 :: Sum
sum1 = (parseSum "2 + x")

equ :: Equation
equ = (parseEquation "2 + 4*x = 6 * (x - 8*(x + 2))")

sume :: Sum
sume = gatherOnOneSide equ
simpleSum1 :: [SimpleTerm]
simpleSum1 = simpleSum sum2
testEq :: Equation
testEq = (parseEquation "5 = x - 2")
sum3 :: Sum
sum3 = gatherOnOneSide (parseEquation "9 = x - 4")
simpleEq :: [SimpleTerm]
simpleEq = simpleSum (gatherOnOneSide equ)

-- a prettier rendering of Rationals
prettyRational :: Rational -> String
prettyRational r
  | denominator r == 1
  = show (numerator r)

  | otherwise
  = "(" ++ show (numerator r) ++ "/" ++ show (denominator r) ++ ")"

-- a SimpleTerm is either a bare number or a coefficient multiplied by x
--data SimpleTerm :: Bool -> Type where
--  Constant :: Rational -> SimpleTerm v
--  VarTerm  :: Rational -> SimpleTerm True
data SimpleTerm where
  SimpleTerm :: Rational     -- the coefficient
       -> Bool       -- True <=> there is an x; False <=> there isn't
       -> SimpleTerm

-- You may wish to uncomment one of these instances for debugging / unit testing:


-- This Show instance is pretty
--instance Show (SimpleTerm v) where
--  show (Constant c) = prettyRational c
--  show (VarTerm c)  = prettyRational c ++ "x"


{-
-- This Show instance is ugly
deriving instance Show (SimpleTerm v)
-}
sumPartHelp :: [Rational] -> Rational
sumPartHelp [] = 0
sumPartHelp [x] = x
sumPartHelp (x:xs) = x + sumPartHelp xs

simp2Rat :: SimpleTerm -> Rational
simp2Rat (SimpleTerm r b) = r

--check x
yesConst :: SimpleTerm -> Bool
yesConst (SimpleTerm r b) = b

noConst :: SimpleTerm -> Bool
noConst (SimpleTerm r b) = not b

intrat :: Integer -> Rational
intrat a = fromInteger a

divss :: [SimpleTerm] -> [SimpleTerm] -> [SimpleTerm]
divss [] [] = []
divss [] y  = y
divss x []  = x
divss [x] xs = map (divHelp x) xs
divss (s1:ss) (s2:sss) = [divHelp s1 s2] ++ divss ss (s2:sss) 

multss :: [SimpleTerm] -> [SimpleTerm] -> [SimpleTerm]
multss [] [] = []
multss [] y  = y
multss x [] = x
multss [x] xs = map (multHelp x) xs
multss (s1:ss) s2 = map (multHelp s1) s2 ++ multss ss s2

multHelp :: SimpleTerm -> SimpleTerm -> SimpleTerm
multHelp (SimpleTerm a1 b1) (SimpleTerm a2 b2) 
    | (b1 == b2 && b1)  = error "!linear"
    | (b1 || b2 )       = (SimpleTerm (a1*a2) True)
    | otherwise         = (SimpleTerm (a1*a2) False)

divHelp :: SimpleTerm -> SimpleTerm -> SimpleTerm
divHelp (SimpleTerm a1 b1) (SimpleTerm a2 b2) 
    | b1 = (SimpleTerm (a1/a2) True)
    | b2 = error "x bottom term of div"
    | otherwise = (SimpleTerm (a1/a2) False)
-- Step 0 ????
checkEquation :: Equation -> Maybe LEquation
checkEquation _ = error "checkEquation unimplemented"

-- Step 0 ???
-- Check to see if a Sum is linear. If the second argument is SFalse,
-- the Sum cannot contain any variables at all.
checkSum :: Sum -> SBool v -> Maybe (LSum v)
checkSum _ = error "checkSum unimplemented"

-- Step 0 for Terms ??
checkTerm :: Term -> SBool v -> Maybe (LTerm v)
checkTerm _ = error "checkTerm unimplemented"

-- Step 0 for Factor
checkFactor :: Factor -> SBool v -> Maybe (LFactor v)
checkFactor _ = error "checkFactor unimplemented"

-- Step 1
gatherOnOneSide :: Equation -> Sum
gatherOnOneSide (Equation l r) = (Minus l r)

plusHelp :: [SimpleTerm] -> [SimpleTerm] -> [SimpleTerm]
plusHelp [] [] = []
plusHelp [] s2 = s2
plusHelp s1 [] = s1
plusHelp ((SimpleTerm a1 b1):s1) ((SimpleTerm a2 b2):s2) 
    | b1 == b2   = [(SimpleTerm (a1+a2) b2)] ++ s1 ++ s2
    | otherwise = [(SimpleTerm a1 b1)] ++ plusHelp s1 ((SimpleTerm a2 b2):s2)

minusHelp :: [SimpleTerm] -> [SimpleTerm] -> [SimpleTerm]
minusHelp [] [] = []
minusHelp y [] = y
minusHelp [] ((SimpleTerm a b):x) = [(SimpleTerm (a*(-1)) b)] ++ minusHelp [] x
minusHelp ((SimpleTerm a1 b1):xs) ((SimpleTerm a2 b2):xss) 
    | b1 == b2   = [(SimpleTerm (a1-a2) b1)] ++ minusHelp xs xss
    | otherwise = [(SimpleTerm a1 b1)] ++ [(SimpleTerm (a2*(-1)) b2)] ++ minusHelp xs xss
-- Simplify a Sum to a list of SimpleTerms (Step 2)
simpleSum :: Sum -> [SimpleTerm]
simpleSum sum = case sum of
  (Plus a b) -> plusHelp (simpleSum a) (simpleSum b)
  (Minus a b) -> minusHelp (simpleSum a) (simpleSum b)
  (Term a) -> simpleTerm a

-- Simplify a Term to a list of SimpleTerms (Step 2)
simpleTerm :: Term -> [SimpleTerm]
simpleTerm term = case term of
  (Mult a b) -> multss (simpleTerm a) (simpleTerm b)
  (Div a b) -> divss (simpleTerm a) (simpleTerm b)
  (Factor a) -> simpleFact a

-- Simplify a Factor to a list of SimpleTerms (Step 2)
simpleFact :: Factor -> [SimpleTerm]
simpleFact f = case f of
  (Lit a) -> [(SimpleTerm (intrat a) False)]
  (Var) -> [(SimpleTerm 1 True)]
  (Sum a) -> simpleSum a

-- Step 3
partitionTerms :: [SimpleTerm]
         -> ( [SimpleTerm]   -- these mention x
          , [SimpleTerm] ) -- these don't
partitionTerms a = (filter yesConst a, filter noConst a)

-- Step 4
sumPartitions :: ( [SimpleTerm], [SimpleTerm] )-> ( Rational, Rational )  -- sum of constants
sumPartitions (x, y) = (sumPartHelp $map simp2Rat x, sumPartHelp $map simp2Rat y)

-- Step 5
extractSolution :: ( Rational       -- coefficient of x, "a"
                   , Rational )     -- constant, "b"
                -> Maybe Rational   -- solution to "a*x + b = 0"
extractSolution _ = error "extractSolution unimplemented"

-- Put them all together
solve :: Equation -> Maybe Rational
solve = extractSolution .
    sumPartitions .
    partitionTerms .
    simpleSum .
    gatherOnOneSide
