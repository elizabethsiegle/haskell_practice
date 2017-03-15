{- CS380 Assignment 3
   Name: Lizzie Siegle
   College email: esiegle@brynmawr.edu
   Resources / collaborators:

   CANNOT FIGURE OUT WHY NEITHER OF THESE WORK FOR CASE 1 = 1

   **DUE ON GRADESCOPE BEFORE CLASS ON WEDNESDAY, FEBRUARY 15, 2017.**
-}


{-# LANGUAGE GADTSyntax, StandaloneDeriving #-}

module Halgebra where

import Arith
import Parser
import Data.Ratio
import Test.HUnit
--------------------------------------------------------------------------------
-- The Halgebra computer algebra system
--

{-
   In this assignment, you will write functions that can be used to solve
   linear equations in one variable, x. The final result will be the `solve`
   function, at the end of this file. `solve`'s type is `Equation -> Rational`;
   it takes an `Equation` in the variable x and returns the `Rational` that
   is a solution to the equation. (`Rational`, exported in the `Prelude` but
   originally from `Data.Ratio`, is a numerical type that can store rational
   numbers of arbitrary precision. By "numerical type", I mean that `Rational`
   in an instance of the `Num` class, meaning that `3 :: Rational` is accepted.)

   This assignment is less prescribed than previous ones, with a few function
   signatures given to you, and the rest left for you to figure out.

   Here is the general approach you will take:

   1. Set one side of the input equation to 0. That is, create a `Sum` that
      evaluates to 0 whenever the original equation holds. (This step
      is really simple!)

   2. Simplify your `Sum` into a list of `SimpleTerm`s. Each `SimpleTerm`
      is a `Rational` coefficient perhaps multiplied by x. This step is done
      by writing the three functions `simpleSum`, `simpleTerm`, and
      `simpleFactor`, which will be mutually recursive. (That is, they
      will call one another.) You will likely need to write several helper
      functions.

   3. Separate out the list of `SimpleTerm`s into those that mention x and
      those that don't.

   4. Add together the coefficients of x and the `SimpleTerm`s that do not
      mention x. Call the former `x_coef` and the latter `constant`.

   5. The solution to the equation is `(-constant)/x_coef`.

   Here is an example:

   Start:  1 + 2*x = 3 * (x - 5*(x + 1))
   After step 1: (1 + 2*x) - (3 * (x - 5*(x + 1)))
   After step 2: [1, 2x, -3x, 15x, 15]
   After step 3: ([2x, -3x, 15x], [1, 15])
   After step 4: (14, 16)
   After step 5: -8/7

   This homework assignment requires the Arith.hs and Parser.hs files as
   posted on our syllabus page. It also requires the `parsec` package. You
   might need to

      cabal install parsec

   to install this on your system.

   Hints:
     * The `fromInteger :: Num a => Integer -> a` function can convert from
       `Integer` to any other numerical type (like `Rational`).

     * By default, `Rational`s print out somewhat oddly in GHCi, using a `%`
       to denote division. I've given you `prettyRational` that does a better
       job.

     * There are three ways solving can fail:
       1) You can have a non-linear equation, where you try to multiply x by x.
       2) You can have a non-linear equation, where you try to divide by x.
       3) All the x's can cancel out.
       In any of these scenarios, just call `error :: String -> a` with an
       appropriate error message. Do *not* try to detect (and accept) an equation
       with x/x in it, such that the division by x is OK. Any division by
       an expression with x in it is rejected.

       (This approach toward failure is regrettable. Really, we should return
        a `Maybe Rational`. But that will complicate things too much at this
        stage. We shall return!)

     * Simplifying (a + b + c) * (d + e + f) means you multiply everything
       in the left sum by everything in the right sum, producing *nine* output
       terms. A list comprehension might be useful.

     * Simplifying (a + b + c) / (d + e + f) is harder. Because we reject any
       denominator mentioning x, we can just add the d, e, and f (which must be
       x-less `SimpleTerm`s), and then divide each of a, b, and c by the sum.

     * Write unit tests! You will thank yourself later.
-}

-- a prettier rendering of Rationals
prettyRational :: Rational -> String
prettyRational r
  | denominator r == 1
  = show (numerator r)

  | otherwise
  = "(" ++ show (numerator r) ++ "/" ++ show (denominator r) ++ ")"

-- a SimpleTerm is a coefficient and, perhaps, an x
data SimpleTerm where
  SimpleTerm :: Rational       -- the coefficient
             -> Bool           -- True <=> there is an x; False <=> there isn't
             -> SimpleTerm

-- You may wish to uncomment one of these instances for debugging / unit testing:


-- This Show instance is pretty
instance Show SimpleTerm where
  show (SimpleTerm coef has_x) = show_coef ++ maybe_show_x
    where
      show_coef = prettyRational coef

      maybe_show_x | has_x     = "x"
                   | otherwise = ""


{-
-- This Show instance is ugly
deriving instance Show SimpleTerm
-}


-- Step 1
gatherOnOneSide :: Equation -> Sum
gatherOnOneSide (Equation sum1 sum2) = (Minus sum1 sum2)

sumSimpleTerm:: SimpleTerm -> (SimpleTerm -> SimpleTerm)
sumSimpleTerm (SimpleTerm co1 bo1) = \(SimpleTerm co2 bo2) -> (SimpleTerm (co1+co2) False)

sumSimpleTerm2 :: SimpleTerm -> SimpleTerm -> SimpleTerm
sumSimpleTerm2 (SimpleTerm co1 bo1) (SimpleTerm co2 bo2) = SimpleTerm (co1 + co2) (bo1 || bo2)
multSimpleTerm::SimpleTerm -> SimpleTerm -> SimpleTerm
multSimpleTerm (SimpleTerm co1 bo1) (SimpleTerm co2 bo2)
    | co1 == 0 && co2 == 0  = error "x cancels x"
    | bo1 && bo2            = error "x*x"
    | otherwise             = SimpleTerm(co1*co2) (bo1 || bo2)

addRat2STermLst ::Rational -> [SimpleTerm] -> SimpleTerm
addRat2STermLst n [] = SimpleTerm n False
addRat2STermLst n l = addRat2STermLst ((addRST n (head l))) (tail l)

--add rational + rational of SimpleTerm
addRST:: Rational -> SimpleTerm ->Rational
addRST n (SimpleTerm r b) = n + r

--return boolean of SimpleTerm
checkBool :: SimpleTerm -> Bool
checkBool (SimpleTerm c b) = b

-- Simplify a Sum to a list of SimpleTerms (Step 2)
simpleSum :: Sum -> [SimpleTerm]
simpleSum (Plus p q) = simpleSum p ++ simpleSum q
simpleSum (Minus p q) = simpleSum p ++ map(multNeg1SimpleTerm) (simpleSum q)
simpleSum (Term t) = simpleTerm t

--helper function for simpleTerm (Div)
divideSimpleTerm :: SimpleTerm -> SimpleTerm -> SimpleTerm
divideSimpleTerm (SimpleTerm co1 bo1) (SimpleTerm co2 bo2) 
    | co2 == 0   = error "first coeff = zero"
    | bo2        = error "divide by x"
    | bo1 && bo2 = error "x cancels out"
    | otherwise  = SimpleTerm(co1/co2) (bo1 || bo2)

--helper function for simpleTerm(Mult)
multNeg1SimpleTerm:: SimpleTerm -> SimpleTerm
multNeg1SimpleTerm (SimpleTerm c b) = SimpleTerm (c * (-1)) b 
-- Simplify a Term to a list of SimpleTerms (Step 2)
simpleTerm :: Term -> [SimpleTerm]
simpleTerm (Mult p q) = [multSimpleTerm x y | x <- (simpleTerm p), y <- (simpleTerm q)]
simpleTerm (Div l r) = [divideSimpleTerm x (addRat2STermLst (fromInteger 0) (simpleTerm r)) | x <- (simpleTerm l)]
simpleTerm (Factor f) = simpleFact f

-- Simplify a Factor to a list of SimpleTerms (Step 2)
simpleFact :: Factor -> [SimpleTerm]
simpleFact (Lit i) = [SimpleTerm (toRational i) False] -- just i
simpleFact (Var)= [SimpleTerm (toRational 1) True] --just x
simpleFact (Sum s) = simpleSum(s)

-- Step 3
--You would generally want to use recursion on the input structure, not the output structure. 
--so you want to use recursion on the tail of the list, not the second list in the tuple.
partitionTerms :: [SimpleTerm]
               -> ( [SimpleTerm]   -- these mention x
                  , [SimpleTerm] ) -- these don't
partitionTerms lst = partTermHelpFunc lst ([], [])
partTermHelpFunc :: [SimpleTerm] -> ([SimpleTerm], [SimpleTerm]) -> ([SimpleTerm], [SimpleTerm])
partTermHelpFunc lst t 
    | length lst == 0  = t
    | checkBool (head lst) = partTermHelpFunc (tail lst) ((head lst) : (fst t), snd t)
    | otherwise         = partTermHelpFunc (tail lst) (fst t, (head lst) : snd t)

-- Step 4
--partTrue::[SimpleTerm] -> Rational
--partTrue [] = 0
--partTrue [SimpleTerm(r: b) ] = r ++ partTrue ([SimpleTerm rs True])
  --let ys = 0 : zipWith (+) ys [r] in last ys
  --toRational (foldr (+) 0 r)
  --partitionTerms [(SimpleTerm 1 False), (SimpleTerm (-1) False)]

sumPartHelp::SimpleTerm -> Rational
sumPartHelp (SimpleTerm r b) = r

--Add together the coefficients of x and the `SimpleTerm`s that do not
    --  mention x. Call the former `x_coef` and the latter `constant`.
sumPartitions :: ( [SimpleTerm]    -- these mention x
                 , [SimpleTerm] )  -- these don't
              -> ( Rational        -- sum of coefficients of x
                 , Rational )      -- sum of constants
sumPartitions ([], []) = (0, 0)
sumPartitions (lst1, lst2) = (sumPartHelp (addRat2STermLst (fromRational 0) lst1), sumPartHelp (addRat2STermLst (fromRational 0) lst2))

-- Step 5
extractSolution :: ( Rational     -- coefficient of x, "a" -constant/coeff
                   , Rational )   -- constant, "b"
                -> Rational       -- solution to "a*x + b = 0"
extractSolution (r1, r2) = -r2/r1

-- Put them all together
solve :: Equation -> Rational
solve = extractSolution .
        sumPartitions .
        partitionTerms .
        simpleSum .
        gatherOnOneSide