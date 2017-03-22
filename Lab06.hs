{-# LANGUAGE GADTs, TypeInType, StandaloneDeriving, TypeFamilies,
             TypeOperators, ScopedTypeVariables, TypeApplications, FlexibleContexts #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Lab06 where   -- rename as you please

import Data.Kind ( Type ) 
import Prelude hiding (last, map, and, or, any, take, unzip, drop, length, init, null, uncons, foldl, replicate, foldl1, 
  scanr, scanr1, foldr, foldr1, scanl, scanl1)

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

type family (a :: Nat) - (b :: Nat) :: Nat where
  Zero    - z         = Zero --natural number needs to be 0 lol cutoff
  z       - Zero      = z
  Succ z1 - (Succ z2) = z1 - z2
infixl 6 -

mymin :: Vec n a -> a 
mymin Nil    = Zero
mymin (x:>Nil) = Zero
mymin (x:>y:>xs) = mymin ((if x < y then x else y):>xs)

-- singleton Nat
data SNat :: Nat -> Type where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

-- singleton Bool
data SBool :: Bool -> Type where
  SFalse :: SBool False
  STrue  :: SBool True


safeHead :: Vec (Succ n) a -> a
safeHead (x :> _) = x
safeTail :: Vec (Succ n) a -> Vec n a
safeTail (_ :> xs) = xs

stuffTrue = True :> True :> True :> Nil
stuffFalse = False :> False :> False :> Nil
stuffPair= (2,4) :> (6,18) :> (10,12) :> Nil
stuffInt = 2:>4:>8:> Nil

{-Consulting the documentation on lists, come up with types and implementations for the translation of the following functions to work on Vecs. There should be no lists in your code.
-}
--1. and
and :: Vec (Succ n) Bool -> Bool
and (x :> Nil) = x
and (x :> xs@(_ :> _))  = x && (and xs)

--2. or
or :: Vec (Succ n) Bool -> Bool
or (x :> Nil) = x
or (x :> xs@(_ :> _)) = x || (or xs)

-- 3. any
any :: (a-> Bool) -> Vec n a -> Bool
any f Nil   = False
any f (x:> b) = case f x of
  True -> True
  False -> any f b

--4. map
map:: (a->b) -> Vec n a -> Vec n b
map _ Nil     = Nil
map f (x:>xs) = f x :> map f xs 

--5. last
last::Vec (Succ n) a -> a
last (x:>Nil) = x
last (x :> y:>ys) = last (y:>ys) --prove ys isn't empty
--last (_:> xs@(_:>_)) = last xs

-- 6. unzip
--transforms a list of pairs into a list of first components and a list of second components.
{-# INLINE unzip #-}
unzip :: Vec n (a,b) -> ( (Vec n a), (Vec n b) )
unzip Nil = (Nil, Nil);
unzip ( x :> xs ) = ( (fst x) :> (fst $ unzip xs), (snd x) :> (snd $ unzip xs))

--7. uncons
--Decompose lst into head, tail. If list non-empty, return Just (x, xs), where x is the head of the list and xs its tail.
uncons :: Vec (Succ n) a -> (a, Vec n a)
uncons (x:> xs) = (x, xs)

-- 8. init: all except last
init :: Vec (Succ n) a -> (Vec n a)
init (x:>Nil) = Nil
init (x :> y:>ys) = x:> init (y:>ys)

insert:: Ord a => a -> (Vec n a) -> Vec (Succ n) a
insert x Nil = x :> Nil
insert x (y :> ys)
  | x < y     = x :> y :> ys
  | otherwise = y :> (insert x ys)

--10 sort (insertion)
insertionSort :: Ord b => (Vec n b) -> (Vec n b)
insertionSort Nil = Nil
insertionSort (x:>xs) = insert x (insertionSort xs)

-- 11. null (this can return an SBool)
null:: (Vec n b) -> Bool --SBool
null Nil     =  True--STrue 
null (_:>_)  =  False --SFalse 

--12. length (this can return an SNat)
length:: (Vec s b) -> SNat s
length (Nil) = SZero
length (_:>xs) = SSucc (length xs)

-- 13. stripPrefix drops the given prefix from a list. It returns Nothing if the list did not start with the prefix given, or Just the list after the prefix, if it does.
stripPrefix :: Eq a => (Vec n a) -> (Vec m a) -> Maybe (Vec (m -n) a)
stripPrefix Nil ys = Just ys
stripPrefix (_ :> _) Nil = Nothing
stripPrefix (x :> xs) (y :> ys)
    | x == y    = stripPrefix xs ys
    | otherwise = Nothing


-- 14. take
--take n xs returns the prefix of xs of length n, with n less than or equal to the length of xs.
take :: SNat n -> (Vec m a) -> Vec (mymin n m) a --(Vec (n-m) a)
--take (SSucc _) Nil       = Nil
take (SSucc n) (x :> xs) = take n xs
--take SZero _             = Nil

-- 15. drop n xs returns the suffix of xs after the first n elements, with n less than or equal to the length of xs.

drop:: SNat m -> Vec n a -> Vec (n-m) a
drop SZero x = x
drop _ Nil   = Nil
drop (SSucc n) (x:>xs) = drop n xs


---- 16. replicate
replicate :: SNat n -> a -> Vec n a
replicate SZero _     = Nil
replicate (SSucc n) a = a :> replicate n a

-- hw
----a
foldl::(a -> b -> a) -> a -> (Vec v b) -> a
foldl _ z Nil = z
foldl f z (x:>xs) = foldl f (f z x) xs

------b
foldl1::(a -> a -> a) -> (Vec n a) -> a
foldl1 _ Nil = error "ugh"
foldl1 _ (x:>Nil) = x
foldl1 f (x:> xs) = foldl f x xs

----c
foldr:: (a -> b -> b) -> b -> Vec n a -> b
foldr f z Nil     =  z
foldr f z (x:>xs) =  f x (foldr f z xs)

------d
foldr1:: (a -> a -> a) -> Vec n a -> a 
foldr1 f (x:>Nil) =  x
foldr1 f (x:>xs)  =  f x (foldr1 f xs)
foldr1 _ Nil      =  error "empty vec"

----e
scanl:: (b -> a -> b) -> b -> (Vec n a) -> (Vec (Succ n) b)
scanl f q Nil      = q:>Nil
scanl f q (x:>xs)  = q:> scanl f (f q x) xs

----f
scanl1:: (a -> a -> a) -> Vec n a -> Vec n a
scanl1 _ Nil = Nil
scanl1 _ (x:> Nil)  = x:>Nil
scanl1 f (x:>xs) = scanl f x xs


----g **COME BACK 
scanr:: (a -> b -> b) -> b -> Vec n a -> Vec (Succ n) b
scanr _ y Nil     = y :> Nil
scanr f y (x:>xs) = f x y :> (scanr f y xs)


----h
--scanr1:: (a -> a -> a) -> Vec n a -> Vec (Succ n) a
--scanr1 _ Nil     =  Nil
--scanr1 f (x:>xs) =  x:>xs
--scanr1 f (x:>xs) =  f x q :> scanr1 f xs 

----i
--mapAccumL :: (acc -> x -> (acc, y)) -> acc -> Vec n a -> (acc, Vec n y)
mapAccumL :: (a->b-> (a, c)) -> a -> Vec n b -> (a, Vec n c)
mapAccumL f s Nil = (s, Nil)
mapAccumL f s (x:>xs) = (s'',y:>ys)
                            where (s', y ) = f s x
                                  (s'',ys) = mapAccumL f s' xs

----j
mapAccumR :: (acc -> x -> (acc, y))    
            -> acc              
            -> Vec n x             
            -> (acc, Vec n y)          
mapAccumR _ s Nil        =  (s, Nil)
mapAccumR f s (x:>xs)    =  (s'', y:>ys)
                           where (s'',y ) = f s' x
                                 (s', ys) = mapAccumR f s xs

----k
--splitAt:: SNat b -> Vec m a -> (Vec Min (m b) a, Vec (m-b) a)
--splitAt n xs   =  (take n xs, drop n xs)
--splitAt n ls
--  | n <= 0 = (Nil, ls)
--  | otherwise   = splitAt n ls
--    where
--        splitAt _  Nil     = (Nil, Nil)
--        splitAt 1  (x:>xs) = (x:>xs, xs)
--        splitAt m  (x:>xs) = (x:>xs', xs'')
--          where
--            (xs', xs'') = splitAt (m - 1) xs
--splitAt:: SNat b -> (Vec n a) -> ((Vec n b), (Vec n b))
--splitAt n xs    =  (take n xs, drop n xs)
--splitAt:: Int -> [a] -> ((Vec n a, [a])
--splitAt :: (n :<<= m) ~ 'True => SNat n -> Vector a m -> (Vector a n, Vector a (m :> n))
--splitAt SZ     xs        = (Nil, xs)
--splitAt (SS n) (x :- xs) =
--  case splitAt n xs of
--    (xs', ys') -> (x :- xs', ys')
--splitAt _ _ = error "could not happen!"