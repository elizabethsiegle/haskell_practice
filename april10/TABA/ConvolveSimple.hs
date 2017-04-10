{-# OPTIONS_GHC -fplugin=TypeNatSolver #-}
--{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, GADTs, KindSignatures, RankNTypes, TupleSections,
             ScopedTypeVariables, UnicodeSyntax, LambdaCase, StandaloneDeriving, PolyKinds #-}

module ConvolveSimple where

import Common

----------------------------
-- Naive list convolution --
----------------------------

-- Zip two equal-length lists together
zipVec :: ∀ n a b. Vec n a -> Vec n b -> Vec n (a,b)
zipVec Nil       Nil       = Nil
zipVec (a :. as) (b :. bs) = (a,b) :. zipVec as bs

-- Reversing a length-indexed list with an accumulator
-- This won't type-check without the type-nats plugin!
reverseVec :: ∀ n a. Vec n a -> Vec n a
reverseVec = go Nil
   where
      go :: ∀ x y b. Vec x b -> Vec y b -> Vec (x + y) b
      go acc Nil       = acc
      go acc (a :. as) = go (a :. acc) as

convolveSimple :: ∀ a b. [a] -> [b] -> [(a,b)]
convolveSimple xs ys =
   case compare (length xs) (length ys) of
      GT -> precondition 1 "lists must be equal length: first list too long"
      LT -> precondition 2 "lists must be equal length: second list too long"
      EQ -> zip xs (reverse ys)

convolveSimple' :: ∀ a b n. Vec n a -> Vec n b -> Vec n (a,b)
convolveSimple' xs ys = zipVec xs (reverseVec ys)
-- invariants [1,2]: satisfied by top-level type, since inputs are equal length
