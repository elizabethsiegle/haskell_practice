{-# LANGUAGE TypeOperators, TypeInType, GADTs, TypeFamilies,
             UndecidableInstances, AllowAmbiguousTypes, ScopedTypeVariables,
             TypeApplications #-}

module Class where
import Data.Kind ( Type )
import Prelude hiding ( (++), concat, (!!), filter, foldr, concatMap, takeWhile, dropWhile)
import qualified Prelude
import Data.Type.Equality

gteRefl :: SNat n -> n :>=: n
gteRefl SZero = GTEZero
gteRefl (SSucc m) = GTESucc(gteRefl m)

gteRefl' :: Vec n a -> n :>=: n
gteRefl' Nil = GTEZero
gteRefl' (_:>xs) = GTESucc (gteRefl' xs)

data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat

type family a + b where
  Zero   + b = b
  Succ a + b = Succ (a + b)

type family (a :: Nat) * (b :: Nat) :: Nat where
   Zero   * b = Zero
   Succ a * b = b + (a * b)

(++) :: Vec n a -> Vec m a -> Vec (n + m) a
Nil       ++ ys = ys
(x :> xs) ++ ys = x :> (xs ++ ys)
infixr 5 ++

data VecList :: [Nat] -> Type -> Type where
   VLNil :: VecList '[] a
   (:>>) :: Vec n a -> VecList ns a -> VecList (n ': ns) a
--infixr 5 :>>

data Vec :: Nat -> Type -> Type where
  Nil :: Vec Zero a
  (:>) :: a -> Vec n a -> Vec (Succ n) a
infixr 5 :>

data SBool:: Bool -> Type where
  SFalse :: SBool False
  STrue :: SBool True

data SNat :: Nat -> Type where
  SZero :: SNat Zero
  SSucc :: SNat n -> SNat (Succ n)

data EVec :: (Nat -> Type) -> Type -> Type where
  EVec :: proof n -> Vec n a -> EVec proof a

data AlwaysTrue :: Nat -> Type where
  Always :: AlwaysTrue n

toVec :: [a] -> EVec AlwaysTrue a   --  toVec :: [a] -> Vec n a
                         --  toVec :: [a] -> (exists n. Vec n a)
toVec [] = EVec Always Nil
toVec (x:xs) = case toVec xs of
  EVec _ ys -> EVec Always (x :> ys)

data Fin :: Nat -> Type where
  FZero :: Fin (Succ n)
  FSucc :: Fin n -> Fin (Succ n)

data n :<=: m where
  LTEZero :: Zero :<=: m
  LTESucc :: n :<=: m -> Succ n :<=: Succ m
data (:>=:) :: Nat -> Nat -> Type where
  GTEZero :: n :>=: Zero
  GTESucc :: n :>=: m -> Succ n :>=: Succ m

--delete help
data (:-:) :: Nat -> Nat -> Type where
  Equ :: n :-: m
  OffByOne :: Succ n :-: m
--delete help  
clone :: Vec n a -> EVec ( (:-:) n) a
clone xs = EVec Equ xs

--delete help
doubleEq ::  (n :-: m) -> (Succ n :-: Succ m)
doubleEq Equ = Equ
doubleEq OffByOne = OffByOne

--helpers
foldr :: (a -> b -> b) -> b -> Vec n a -> b
foldr _ x Nil = x
foldr f x ( y :> ys ) = (f y (foldr f x ys))

gteSuccL :: (n :>=: m) -> (Succ n :>=: m)
gteSuccL GTEZero       = GTEZero
gteSuccL (GTESucc gte) = GTESucc (gteSuccL gte)

dweHelp ::  EVec AlwaysTrue a -> Bool
dweHelp  (EVec Always Nil) = True

nubHelp1 :: Eq b => b -> Vec n b -> Bool
nubHelp1 _ Nil = False
nubHelp1 a (x:>xs) 
  | a == x = True
  | otherwise = False || nubHelp1 a xs
nubHelp2 :: EVec AlwaysTrue a -> EVec AlwaysTrue a -> EVec AlwaysTrue a
nubHelp2 (EVec t vec1) (EVec t1 vec2) = EVec Always (vec1 ++ vec2)

unfoldrHelp ::Maybe (x,y) -> y
unfoldrHelp (Just (x,y)) = y

elemIndicesHelp :: Vec n (Fin a) -> Vec n (Fin (Succ a))
elemIndicesHelp Nil = Nil
elemIndicesHelp (x :> xs) = (FSucc x) :> elemIndicesHelp xs

type family NatToList (n:: Nat) (m:: Nat) :: [Nat] where
  NatToList n Zero      = '[]
  NatToList n (Succ m') = n : NatToList n m'

--a. concatMap::(a -> [b]) -> [a] -> [b]--1. concatMap
concatMap :: (a ->EVec AlwaysTrue b) -> Vec n a -> EVec AlwaysTrue b
concatMap _ Nil = EVec Always Nil
concatMap f (x :> xs) = case concatMap f xs of EVec Always xs' -> case f x of EVec Always ys' -> EVec Always (xs'++ys')

--b. unfoldr::(b -> Maybe (a, b)) -> b -> [a]
unfoldr :: (b -> Maybe (a,b)) -> b -> EVec AlwaysTrue a
unfoldr f b = case unfoldr f (unfoldrHelp(f b)) of x@(EVec Always xs') -> case f b of
                                                                               Nothing   -> x
                                                                               Just(a,b) -> EVec Always (xs' ++ (a :> Nil));
-- c.takeWhile
takeWhile :: (a -> Bool) -> Vec m a -> EVec ((:>=:) m) a
takeWhile _ Nil = EVec GTEZero Nil
takeWhile f (x:>xs) = case takeWhile f xs of
                                             EVec gte xs'
                                               | f x -> EVec (GTESucc gte)  (x :> xs')
                                               | otherwise -> EVec (gteSuccL gte) xs'
--d. dropWhile:: (a -> Bool) -> [a] -> [a]
dropWhile :: (a -> Bool) -> Vec n a -> EVec AlwaysTrue a
dropWhile _ Nil = EVec Always Nil
dropWhile f xs@(x :> xs')
     | f x = dropWhile f xs'
     | otherwise = EVec Always xs 

--e. dropWhileEnd:: (a -> Bool) -> [a] -> [a]
dropWhileEnd :: (a -> Bool) -> Vec n a -> EVec AlwaysTrue a
dropWhileEnd _ Nil = EVec Always Nil
dropWhileEnd f xs = foldr (\x xs'@(EVec Always ys) -> if f x && (dweHelp  xs')  then (EVec Always Nil) else (EVec Always (x :> ys))) (EVec Always Nil) xs

--f. filter
filter :: (a -> Bool) -> Vec n a -> EVec AlwaysTrue a
filter _ Nil       = EVec Always Nil
filter f (x :> xs) = case filter f xs of
  EVec _ fxs -> if f x then EVec Always (x :> fxs)
                else EVec Always fxs

--g. (!!) (uses Fin)
(!!) :: Vec n a -> Fin n -> a
vec !! fin = case (fin, vec) of
  (FZero,   x :> _)  -> x
  (FSucc n, _ :> xs) -> xs !! n

--h. elemIndex:: Eq a => a -> [a] -> Maybe Int (uses Fin)
elemIndex :: Eq a => a -> Vec n a -> Maybe (Fin n)
elemIndex _ Nil = Nothing 
elemIndex a (x :> xs)
          | a == x = Just FZero
          | otherwise = case elemIndex a xs of Just f -> Just (FSucc f) 

--i. elemIndices:: Eq a => a -> [a] -> [Int] (uses EVec and Fin)
elemIndices :: Eq a => a -> Vec n a -> EVec ((:>=:)n) (Fin n)
elemIndices _ Nil = EVec GTEZero Nil
elemIndices a (x :> xss) -- how to identify when is the head of Vec n a -> then f should be FZero, since it is input
               | a == x = case elemIndices a xss of EVec gt ys -> EVec (GTESucc gt) (FZero :> (elemIndicesHelp ys))
               | otherwise = case elemIndices a xss of EVec gt ys -> EVec (gteSuccL gt) (elemIndicesHelp ys) 

--findIndex:: (a -> Bool) -> [a] -> Maybe Int (uses Fin)
findIndex :: (a -> Bool) -> Vec m a -> Maybe (Fin m)
findIndex _ Nil = Nothing
findIndex f (x :> xs)
            | f x = Just FZero
            | otherwise = case findIndex f xs of Just f -> Just (FSucc f)

--findIndices:: (a -> Bool) -> [a] -> [Int] (uses EVec and Fin)
findIndices :: (b -> Bool) -> Vec m b -> EVec ((:>=:) m) (Fin m)
findIndices _ Nil = EVec GTEZero Nil
findIndices f (x :> xs)
               | f x = case findIndices f xs of EVec gte ys -> EVec (GTESucc gte) (FZero :> (elemIndicesHelp ys))
               | otherwise = case findIndices f xs of EVec gte ys -> EVec (gteSuccL gte) (elemIndicesHelp ys) 
--nub:: Eq a => [a] -> [a]
--nub :: (Eq a) => Vec n a -> EVec AlwaysTrue a
nub :: Eq a => Vec m a -> EVec AlwaysTrue a
nub zs = nub' zs Nil
  where
    nub' :: Eq a => Vec m a -> Vec n a -> EVec AlwaysTrue a
    nub' Nil _ = EVec Always Nil
    nub' (x:>xs) ys | nubHelp1 x ys = nub' xs ys
                    | otherwise    = nubHelp2 (EVec Always (x:>Nil)) (nub' xs (x:>ys))
--delete:: Eq a => a -> [a] -> [a] (this can get a tighter constraint than, say, filter)
delete :: (Eq a) => a -> Vec n a -> EVec ( (:-:) n) a
delete a ( x :> xs )
       | a == x = case clone xs of EVec Equ ys -> EVec OffByOne ys
       | otherwise = case delete a xs of EVec prev ys -> EVec (doubleEq prev) (x :> ys)
delete _ Nil = EVec Equ Nil
--(\\):: Eq a => [a] -> [a] -> [a]
--union:: Eq a => [a] -> [a] -> [a]
--intersect:: Eq a => [a] -> [a] -> [a]

--span
--break
--partition
--group