{-# LANGUAGE ImplicitParams #-}

------------------------------------------------------------------------------------------------
-- |
-- Module      :  Pearl.GaDtTLHT.Internal.Hom3D
-- Description :                          Homs and Unhoms
-- Copyright   :  (c) Drew Day 2012
--                (c) Shin-Cheng Mu 2011
--                (c) Akimasa Morihata 2011
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Drew Day <drewday@gmail.com>
-- Stability   :  stable
-- Portability :  portable (ViewPatterns, PatternGuards)
--
------------------------------------------------------------------------------------------------
module Pearl.GaDtTLHT.Internal.Hom3D where




hhh :: ((b,b,b) -> b) -> (a -> b) -> b -> [a] -> b
hhh f k e []       = e
hhh f k e [x]      = k x
hhh f k e l@(x:xs) = f (h (reverse xs), h [x], h xs)
               where h = hhh f k e



hs3 xs = hhh f k e xs
          where
                f (l,c,r) = l + c + r + 2*sqrt(l*c + c*r + r*l)
                k x     = x
                e       = 0




