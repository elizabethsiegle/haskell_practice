{-# OPTIONS_GHC -fplugin=TypeNatSolver #-}
--{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, GADTs, KindSignatures, RankNTypes, TupleSections,
             ScopedTypeVariables, UnicodeSyntax, LambdaCase, StandaloneDeriving, PolyKinds #-}

module PalindromeDirect where

import Common

----------------------------------------
-- Palindrome predicate, direct style --
----------------------------------------

-- Whereas Danvy & Goldberg use exceptions to signal non-palindromicity,
-- we use the Maybe monad to the same effect.

palindromeDirect :: ∀ a. Eq a => [a] -> Bool
palindromeDirect xs =
   case walk xs xs of
      Nothing -> False
      Just [] -> True
      Just (_ : _) ->
         impossible "invariant [1]" -- (walk xs xs) always returns Just [] or Nothing
   where
      walk :: ∀ b. Eq b => [b] -> [b] -> Maybe [b]
      walk xs1                 []     = Just xs1  -- even length
      walk (_ : xs1')     (_ : [])    = Just xs1' --  odd length
      walk (x : xs1') (_ : _ : xs2'') =
         walk xs1' xs2'' >>= \case
            (y : ys) -> guard (x == y) >> return ys
            [] -> impossible "invariant [2]"  -- xs2 is shrinks 2x faster than xs1,
      walk [] (_ : _) =                       -- so base case never returns []
         impossible "invariant [3]" -- length xs1 ≥ length xs2"

palindromeDirect' :: ∀ a n. Eq a => Vec n a -> Bool
palindromeDirect' xs =
   case walk xs xs of
      Nothing  -> False
      Just Nil -> True   -- invariant [1]: n - m + p = n - 2m + p ⇔ m = 0
   where                 -- therefore, this is an exhaustive match
      walk :: ∀ m. Vec (n - m)     a
                -> Vec (n - m * 2) a
                -> Maybe (Vec m a)
      walk       xs1              Nil    = Just xs1  -- even length
      walk (_ :. xs1')      (_ :. Nil)   = Just xs1' --  odd length
      walk (x :. xs1') (_ :. _ :. xs2'') =
         walk xs1' xs2'' >>= \(y :. ys) ->  -- invariant [2]: ∀ m, n ∈ ℕ. m > 0 ⇔ n - m + p > n - 2m + p
            guard (x == y) >> return ys     -- therefore the list is non-empty
      -- invariant [3]: ∀ n, m, p ∈ ℕ: n - m + p ≥ n - 2m + p
      -- therefore xs1 cannot be shorter than xs2
