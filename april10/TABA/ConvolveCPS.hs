{-# OPTIONS_GHC -fplugin=TypeNatSolver #-}
--{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, GADTs, RankNTypes, TupleSections,
             ScopedTypeVariables, UnicodeSyntax, LambdaCase, PolyKinds #-}

module ConvolveCPS where

import Common

--------------------------------
-- CPS-style list convolution --
--------------------------------

convolveCPS :: ∀ a b. [a] -> [b] -> [(a,b)]
convolveCPS xs ys =
   walk xs $ \r l ->
      case l of
         [] -> r
         _ : _ ->
            precondition 1 "we only have leftovers if length xs < length ys"
   where
      walk :: ∀ c. [c] -> ([(c,b)] -> [b] -> [(c,b)]) -> [(c,b)]
      walk      []  k = k [] ys
      walk (a : as) k =
         walk as $ \r bs ->
            case bs of
               b : bs' -> k ((a,b) : r) bs'
               [] -> precondition 2 "we only run out of bs if length as > length bs"

convolveCPS' :: ∀ a b n. Vec n a -> Vec n b -> Vec n (a,b)
convolveCPS' xs ys =
   walk xs (\r Nil -> r)  -- precondition [1]: ∀ n ∈ ℕ, n - m = 0 implies m = n
   where                  -- therefore the output from walk must be of length n
      walk :: ∀ m c. (m <= n)
           => Vec m c
           -> (Vec m (c,b) -> Vec (n - m) b -> Vec n (c,b))
           -> Vec n (c,b)
      walk      Nil  k = k Nil ys
      walk (a :. as) k =
         walk as $ \r bs ->
            case bs of                         -- precondition [2]: ∀ n, m ∈ ℕ, n - m + 1 > 0
               b :. bs' -> k ((a,b) :. r) bs'  -- therefore the list is non-empty
