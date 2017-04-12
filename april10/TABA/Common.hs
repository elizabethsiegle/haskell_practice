{-# OPTIONS_GHC -fplugin=TypeNatSolver #-}
--{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, GADTs, KindSignatures, RankNTypes, TupleSections,
             ScopedTypeVariables, UnicodeSyntax, LambdaCase, StandaloneDeriving, PolyKinds, ImplicitParams,
             ConstraintKinds, AllowAmbiguousTypes #-}

module Common
   ( module GHC.TypeLits
   , module Data.Type.Equality
   , module Data.Proxy
   , module Control.Monad
   , Vec(..)
   , impossible
   , precondition
   , hint
   ) where

import GHC.TypeLits
import Data.Type.Equality
import Data.Proxy
import Control.Monad
import Prelude hiding ( iterate )
import GHC.Exception

--------------------------------------------
-- Prelude: length-indexed lists with SMT --
--------------------------------------------

data Vec n a where
   Nil  :: Vec 0 a
   (:.) :: (0 <= n) => a -> Vec n a -> Vec (1 + n) a

instance Functor (Vec n) where
   fmap _      Nil  = Nil
   fmap f (x :. xs) = f x :. fmap f xs

infixr 5 :.
deriving instance Show a => Show (Vec n a)

-------------------
-- Miscellaneous --
-------------------

impossible :: String -> a
impossible = error . ("impossible: " ++)

precondition :: Int -> String -> a
precondition n = error . (\s -> "precondition [" ++ show n ++ "]: " ++ s)

-- Give a hint; that is, "please prove this"
-- Most useful with type application, e.g. hint @(x ~ y) ...
hint :: forall c r. c => (c => r) -> r
hint r = r
