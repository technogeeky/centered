{-# LANGUAGE ImplicitParams #-}

------------------------------------------------------------------------------------------------
-- |
-- Module      :  Pearl.GaDtTLHT.Pointfree
-- Description :                   Pointfree Code
-- Copyright   :  (c) Drew Day 2012
--                (c) Shin-Cheng Mu 2011
--                (c) Akimasa Morihata 2011
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Drew Day <drewday@gmail.com>
-- Stability   :  stable
-- Portability :  portable (ViewPatterns, PatternGuards)
--
------------------------------------------------------------------------------------------------
module Pearl.GaDtTLHT.Internal.Pointfree where


infix 6  /\
-- | The infix split combinator.
(/\) :: (a -> b) -> (a -> c) -> a -> (b,c)
(/\) f g x = (f x, g x)

infix 7  ><
(><) :: (l -> r) -> (u -> d) -> (l,u) -> (r,d)
(f >< g) (x,y) = (f x, g y)

infix 7  |><|
(|><|) :: (l -> r) -> (u -> d) -> (l,u) -> (r,d)
f |><| g = f . fst /\ g . snd



infix 4 \/
-- | The infix either combinator.
(\/) :: (b -> a) -> (c -> a) -> Either b c -> a
(\/) = either




