{-# OPTIONS_GHC -fplugin=TypeNatSolver #-}
--{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, GADTs, KindSignatures, RankNTypes, TupleSections,
             ScopedTypeVariables, UnicodeSyntax, LambdaCase, StandaloneDeriving, PolyKinds #-}

module ConvolveDirect where

import Common

-----------------------------------
-- Direct-style list convolution --
-----------------------------------

convolveDirect :: ∀ a b. [a] -> [b] -> [(a,b)]
convolveDirect xs ys =
   case walk xs of
      (r, []) -> r
      (_, _ : _) ->
         precondition 1 "we only have leftovers if length xs < length ys"
   where
      walk :: ∀ c. [c] -> ([(c,b)], [b])
      walk      []  = ([], ys)
      walk (a : as) =
         case walk as of
            (r, b : bs) -> ((a,b) : r, bs)
            (_, []) ->
               precondition 2 "we only run out of bs if length as > length bs"

convolveDirect' :: ∀ a b n. Vec n a -> Vec n b -> Vec n (a,b)
convolveDirect' xs ys =
   case walk xs of
      (r, Nil) -> r  -- precondition [1]: ∀ n ∈ ℕ, m = n implies n - m = 0
   where             -- therefore this is an exhaustive match
      walk :: ∀ m c. (m <= n) => Vec m c -> (Vec m (c,b), Vec (n - m) b)
      walk      Nil  = (Nil, ys)
      walk (a :. as) =
         case walk as of                      -- precondition [2]: ∀ n, m ∈ ℕ, n - m + 1 > 0
            (r, b :. bs) -> ((a,b) :. r, bs)  -- therefore the list is non-empty
