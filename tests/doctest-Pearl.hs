------------------------------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Drew Day 2012
--                (c) Shin-Cheng Mu 2011
--                (c) Akimasa Morihata 2011
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Drew Day <drewday@gmail.com>
-- Stability   :  stable
-- Portability :  portable (ViewPatterns, PatternGuards)
--
------------------------------------------------------------------------------------------------
module Main where

import Test.DocTest

main :: IO ()
main = doctest [
                 "--optghc=-packageghc"
               , "--optghc=-isrc"
               , "--optghc=-idist/build/autogen/"
               , "--optghc=-optP-include"
               , "--optghc=-optPdist/build/autogen/cabal_macros.h"
               , "src/Pearl/GaDtTLHT/Pearl.hs"
               , "src/Pearl/GaDtTLHT/Section01.hs"
               , "src/Pearl/GaDtTLHT/Section02.hs"
               , "src/Pearl/GaDtTLHT/Section03.hs"
               , "src/Pearl/GaDtTLHT/Section04.hs"
               , "src/Pearl/GaDtTLHT/Section05.hs"
               , "src/Pearl/GaDtTLHT/Section06.hs"
               , "src/Pearl/GaDtTLHT/Ref.hs"
               ]

