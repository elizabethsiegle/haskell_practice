{-# OPTIONS_GHC -fplugin=TypeNatSolver #-}
--{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, GADTs, KindSignatures, RankNTypes, TupleSections,
             ScopedTypeVariables, UnicodeSyntax, LambdaCase, StandaloneDeriving, PolyKinds, TypeApplications #-}

module ConvolveHalves where

import Common

---------------------------------
-- Convolving halves of a list --
---------------------------------

-- This is modified from the version in the original paper.
-- Instead of requiring a list-length to be explicitly passed into the function,
-- we use `as' itself as an unary length, making sure we don't touch it by using
-- a fresh type variable in the helper function `walk'.

convolveHalves :: ∀ a. [a] -> [(a,a)]
convolveHalves as =
   case walk as as of
      (r,     [])  -> r
      (_, (_ : _)) ->
         impossible "invariant [1]" -- if length us = length xs, the second projection here must be []
   where
      walk :: ∀ b c. [b] -> [c] -> ([(c, c)], [c])
      walk          []       xs  = ([], xs)
      walk (_ : _ : us) (x : xs) =
         case walk us xs of
            (r, y : ys) -> ((x,y) : r, ys)
            (_, []) -> impossible "invariant [2]"  -- returned Vec can't be [] because we only go halfway down
      walk     (_ : [])  _ = impossible "precondition [3]" -- we will never call walk on odd-length lists
      walk (_ : _ :  _) [] = impossible "precondition [4]" -- we will never call walk on us longer than xs

convolveHalves' :: ∀ a n. Vec (2 * n) a -> Vec n (a,a)
convolveHalves' as =
   case walk as as of
      (r, Nil) -> r   -- invariant [1]: ∀ m, l ∈ ℕ. 2m = l ⇔ l - 2m = 0
   where              -- therefore this is an exhaustive match
      walk :: ∀ l m b c. (2 * m <= l)            -- precondition [4]: we require that 2m ≤ l
           => Vec (2 * m) b -> Vec l c           -- precondition [3]: ∀ m ∈ ℕ. 2m is even
           -> (Vec m (c, c), Vec (l - 2 * m) c)
      walk           Nil        xs  = (Nil, xs)
      walk (_ :. _ :. us) (x :. xs) =
        hint @((2 * m - 2) ~ (2 * (m - 1))) $  -- a hint to the solver: try to prove this equality
          case walk us xs of                   -- invariant [2]: ∀ l > 2m ∈ ℕ. l - 2m + 1 > 0
             (r, y :. ys) -> ((x,y) :. r, ys)  -- therefore the list is non-empty
