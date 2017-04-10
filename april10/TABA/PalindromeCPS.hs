{-# OPTIONS_GHC -fplugin=TypeNatSolver #-}
--{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, GADTs, KindSignatures, RankNTypes, TupleSections,
             ScopedTypeVariables, UnicodeSyntax, LambdaCase, StandaloneDeriving, PolyKinds #-}

module PalindromeCPS where

import Common

-------------------------------------
-- Palindrome predicate, CPS style --
-------------------------------------

palindromeCPS :: ∀ a. Eq a => [a] -> Bool
palindromeCPS xs =
   walk xs xs $ \case
      [] -> True
      _ : _ ->
         impossible "invariant [1]" -- for any xs, walk xs xs k calls k on []
   where
      walk :: ∀ b. Eq b => [b] -> [b] -> ([b] -> Bool) -> Bool
      walk      xs1           []   k = k xs1  -- even length
      walk (_ : xs1)     (_ : [])  k = k xs1  --  odd length
      walk (x : xs1) (_ : _ : xs2) k =
         walk xs1 xs2 $ \case
            (y : ys) -> x == y && k ys
            [] -> impossible "invariant [2]"  -- xs2 is shrinks 2x faster than xs1,
      walk [] (_ : _) _ =                     -- so base case never returns []
         impossible "invariant [3]" -- length xs1 ≥ length xs2"

palindromeCPS' :: ∀ a n. Eq a => Vec n a -> Bool
palindromeCPS' xs =
   walk xs xs (\Nil -> True) -- invariant [1]: n - m + p = n - 2m + p ⇔ m = 0
   where                     -- therefore the output from walk must be of length n
      walk :: ∀ m p. Vec (n - m)     a
                  -> Vec (n - m * 2) a
                  -> (Vec m a -> Bool)
                  -> Bool
      walk       xs1              Nil    k = k xs1   -- even length
      walk (_ :. xs1')      (_ :. Nil)   k = k xs1'  --  odd length
      walk (x :. xs1') (_ :. _ :. xs2'') k =
         walk xs1' xs2'' $                -- invariant [2]: ∀ m, n ∈ ℕ. m > 0 ⇔ n - m + p > n - 2m + p
            \(y :. ys) -> x == y && k ys  -- therefore the list is non-empty
      -- invariant [3]: ∀ n, m, p ∈ ℕ: n - m + p ≥ n - 2m + p
      -- therefore xs1 cannot be shorter than xs2
