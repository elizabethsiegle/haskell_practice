{-# OPTIONS_GHC -fplugin=TypeNatSolver #-}
--{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE DataKinds, TypeOperators, TypeFamilies, GADTs, KindSignatures, RankNTypes, TupleSections,
             ScopedTypeVariables, UnicodeSyntax, LambdaCase, StandaloneDeriving, PolyKinds #-}

--------------------------------------------------------
---- "There and Back Again" and What Happened After ----
------ Or, Fancy Types Don't Require Simple Terms ------
--------------------------------------------------------

module TABA where

import Common

----------------------------------------------------------------------
-- See: Danvy, O. & Goldberg, M. "There and Back Again," ICFP 2002. --
-- Transcribed from Standard ML to Haskell, adding fancier types    --
----------------------------------------------------------------------

import ConvolveSimple   ( convolveSimple   , convolveSimple'   )
import ConvolveDirect   ( convolveDirect   , convolveDirect'   )
import ConvolveCPS      ( convolveCPS      , convolveCPS'      )
import PalindromeDirect ( palindromeDirect , palindromeDirect' )
import PalindromeCPS    ( palindromeCPS    , palindromeCPS'    )
import ConvolveHalves   ( convolveHalves   , convolveHalves'   )
