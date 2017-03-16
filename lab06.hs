{-# LANGUAGE GADTs, TypeInType, StandaloneDeriving, TypeFamilies,
             TypeOperators, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Lab06 where   -- rename as you please

import Data.Kind ( Type ) 
import Prelude hiding (last, map, and, or, any, take, drop, length, init)

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
--1. and
and :: Vec n Bool -> Bool
and Nil    = True
and (x :> b) = and b
--and _      = False  

--2. or
or :: Vec n Bool -> Bool
or Nil      = True
or (x :> b) = or b
--or _        = True

-- 3. any
any :: (a-> Bool) -> Vec n a -> Bool
any f Nil   = False
any f (x:> b) = case f x of
  True -> True
  False -> any f b

--4. map
map:: (a->b) -> Vec n a -> Vec n b
map f Nil     = Nil
map f (x:>xs) = f x :> map f xs 

--5. last
last::Vec (Succ n) a -> a
last (x:>Nil) = x
last (x :> y:>ys) = last (y:>ys) --prove ys isn't empty
--last (_:> xs@(_:>_)) = last xs

-- 6. unzip
unzip    :: [(a,b)] -> ([a],[b])
--transforms a list of pairs into a list of first components and a list of second components.
{-# INLINE unzip #-}
unzip    =  foldr (\(a,b) ~(as,bs) -> (a:as,b:bs)) ([],[])

--7. uncons
--uncons:: (x:>xs) -> Maybe (a, (x:>xs))

-- 8. init
init:: (Vec n b) -> (Vec n b)
init (y:>ys)  =  (y:>ys)
init (y:>ys)  =  y :> init ys
init Nil      =  error "init"

--9. insert
--10 sort (insertion)

-- 11. null (this can return an SBool)
null:: (Vec n b) -> SBool Bool
null Nil      =  STrue True
null (_:>_)    =  SFalse False

--12. length (this can return an SNat)
length:: (Vec n b) -> Int
length l                =  len l 0
  where
    len :: (a:>as) -> Int -> Int
    len []     a  = I a
    len (_:xs) a  = len xs (a + 1)

-- 13. stripPrefix

-- 14. take
take :: SNat s -> (Vec n a) -> (Vec n a)
take i Nil               = Nil
take 0 (x:>xs)           = (x:xs)
take i (y:>ys)           =  y :> take SNat(Succ i) ys

-- 15. drop
drop :: SNat s -> Vec n a -> Vec n a
drop 0 xs           = xs
drop _ Nil          = Nil
drop i (x:> xs)     = x : take SNat(Succ i) xs

-- 16. replicate
replicate :: SNat s -> a -> Vec l a
replicate n x    =  Vec (take n (repeat x))