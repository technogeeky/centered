------------------------------------------------------------------------------------------------
-- |
-- Module      :  Pearl.GaDtTLHT.Theorems
-- Description :                 Theorems
-- Copyright   :  (c) Drew Day 2012
--                (c) Shin-Cheng Mu 2011
--                (c) Akimasa Morihata 2011
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Drew Day <drewday@gmail.com>
-- Stability   :  stable
-- Portability :  portable (ViewPatterns, PatternGuards)
--
-- All of the theorems are collected here, in one place, for convenience.
------------------------------------------------------------------------------------------------
module Pearl.GaDtTLHT.Theorems
     (
------------------------------------------------------------------------------------------------
     -- Section 1
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
     -- * Section 2
------------------------------------------------------------------------------------------------
     -- ** 't01' Second LHT
       t01
     -- *** Proof
     , t01proof
     -- ** 't02' Third LHT
     , t02
     -- *** Proof
     , t02proof
     -- *** Comments
     , t02comments
------------------------------------------------------------------------------------------------
     -- * Section 3
------------------------------------------------------------------------------------------------
     -- ** 't03' Lemma 3
     , t03
     -- *** Proof
     , t03proof

     -- ** 't04' Third LHT (on Relations)
     , t04
     -- *** Proof
     , t04proof

     -- ** 't05' Dual to Third LHT
     , t05
     -- *** Proof
     , t05proof

     -- ** 't06' Lemma 6
     , t06
     -- *** Proof
     , t06proof
------------------------------------------------------------------------------------------------
     -- Section 4
------------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------------
     -- * Section 5
------------------------------------------------------------------------------------------------
     -- ** 't07' Unnamed Theorem
     , t07
     -- *** Proof
     , t07proof
     -- *** Comments
     , t07comments
------------------------------------------------------------------------------------------------
     -- Section 6
------------------------------------------------------------------------------------------------
     )    where


import Pearl.GaDtTLHT.Section02 
                              (
                                t01
                              , t01proof
                              , t02
                              , t02proof
                              , t02comments
                              )

import Pearl.GaDtTLHT.Section03
                              (
                                t03
                              , t03proof
                              , t04
                              , t04proof
                              , t05
                              , t05proof
                              , t06
                              , t06proof
                              )


import Pearl.GaDtTLHT.Section05
                              (
                                t07
                              , t07proof
                              , t07comments
                              )
                              
t11 = undefined
-- ^
-- #11#   K. Morita, A. Morihata, K. Matsuzaki, Z. Hu, and M. Takeichi.  Automatic inversion generates divide-and-conquer parallel programs. In J. Ferrante and K. S. McKinley, editors, Proceedings of the ACM SIGPLAN 2007 Conference on Programming Language Design and Implementation, San Diego, California, USA, June 10-13, 2007, pages 146-155. ACM, 2007.
--
-- [@PDF: <http://takeichi.ipl-lab.org/pub/PLDI2007.pdf>@]
