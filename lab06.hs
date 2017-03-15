{-# LANGUAGE GADTs, TypeInType, StandaloneDeriving, TypeFamilies,
             TypeOperators, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Lab06 where   -- rename as you please

import Data.Kind ( Type ) 
import Prelude hiding (last, map, and, or, any, take, drop)

data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat

data Vec :: Nat -> Type -> Type where
  Nil  :: Vec Zero a
  (:>) :: a -> Vec n a -> Vec (Succ n) a
infixr 5 :>

deriving instance Show a => Show (Vec n a)

type family (a :: Nat) + (b :: Nat) :: Nat where
  Zero   + b = b
  Succ a + b = Succ (a + b)
infixl 6 +

-- singleton Nat
data SNat :: Nat -> Type where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

-- singleton Bool
data SBool :: Bool -> Type where
  SFalse :: SBool False
  STrue  :: SBool True

{-Consulting the documentation on lists, come up with types and implementations for the translation of the following functions to work on Vecs. There should be no lists in your code.
-}
--1. last
last::Vec (Succ n) a -> a
last (x:>Nil) = x
last (x :> y:>ys) = last (y:>ys) --prove ys isn't empty
--last (_:> xs@(_:>_)) = last xs

--2. map
map:: (a->b) -> Vec n a -> Vec n b
map f Nil     = Nil
map f (x:>xs) = f x :> map f xs 

--3. and
and :: Vec n Bool -> Bool
and Nil    = True
and (x :> b) = and b
--and _      = False  

--4. or
or :: Vec n Bool -> Bool
or Nil      = True
or (x :> b) = or b
--or _        = True

-- 5. any
any :: (a-> Bool) -> Vec n a -> Bool
any f Nil   = False
any f (x:> b) = case f x of
  True -> True
  False -> any f b

-- 6. take
take :: Int -> Vec n a -> Vec n b
take 0 _                =  Nil
take _ Nil              =  Nil
take i (x:>xs)          =  x :> take (i-1) xs
--take i Nil = Nil
--take Nil i = Nil
--take i (x:> xs) =
--  | i == 0    = Nil
--  | otherwise = 

-- 7. drop
--drop :: Int -> Vec n a -> Vec n b 

--init
--uncons
--null (this can return an SBool)
--length (this can return an SNat)
