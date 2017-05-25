-- LArith.hs
-- Defines GADTs for storing linear arithmetic expressions
-- Scroll down to find "Show" instances that might need to be uncommented

{-# LANGUAGE GADTs, StandaloneDeriving, TypeInType, TypeOperators #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module LArith where

import Data.Kind ( Type )

data SBool :: Bool -> Type where
  SFalse :: SBool False
  STrue  :: SBool True
deriving instance Show (SBool b)

-- All of these types are parameterized by a Bool saying whether or
-- not they might contain a variable. Something of type `L??? True`
-- may or may not have a variable. Something of type `L??? False`
-- surely does *not* have a variable.

data LSum :: Bool -> Type where
  LPlus  :: LSum v -> LSum v -> LSum v
  LMinus :: LSum v -> LSum v -> LSum v
  LTerm  :: LTerm v -> LSum v

data LTerm :: Bool -> Type where
    -- The three LMult variants say where the x might be found:
    -- on the (L)eft, on the (R)ight, or (N)either
  LMultL  :: LTerm True  -> LTerm False -> LTerm True --x * 4 + 5
  LMultR  :: LTerm False -> LTerm True  -> LTerm True -- whole term have var
  LMultN  :: LTerm False -> LTerm False -> LTerm False -- no var = constant
  LDiv    :: LTerm v -> LTerm False -> LTerm v
  LFactor :: LFactor v -> LTerm v

data LFactor :: Bool -> Type where -- 5 + x needs to be LFactor
  LLit :: Integer -> LFactor v --Lit can be true to line up
  LVar :: LFactor True            -- always "x"
  LSum :: LSum v -> LFactor v

-- an LEquation has a left-hand side and a right-hand side      == vars on both side of eq
data LEquation where
  LEquation :: LSum True -> LSum True -> LEquation

-- You may wish to uncomment one set of Show instances below,
-- depending on whether you want pretty-printing or ugly-printing.

{-
-- These instances tell GHCi to pretty-print the types above
instance Show (LSum v) where
  show (LPlus a b)  = show a ++ " + " ++ show b
  show (LMinus a b) = show a ++ " - " ++ show b
  show (LTerm t)    = show t

instance Show (LTerm v) where
  show (LMultL a b) = show a ++ " * " ++ show b
  show (LMultR a b) = show a ++ " * " ++ show b
  show (LMultN a b) = show a ++ " * " ++ show b
  show (LDiv a b)   = show a ++ " / " ++ show b
  show (LFactor f)  = show f

instance Show (LFactor v) where
  show (LLit l)  = show l
  show LVar      = "x"
  show (LSum s)  = "(" ++ show s ++ ")"

instance Show LEquation where
  show (LEquation lhs rhs) = show lhs ++ " = " ++ show rhs
-}

{-
-- These instances tell GHCi to print out the internal structure
-- of the types above

deriving instance Show (LSum v)
deriving instance Show (LTerm v)
deriving instance Show (LFactor v)
deriving instance Show LEquation
-}