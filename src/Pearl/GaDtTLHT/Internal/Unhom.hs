{-# LANGUAGE ImplicitParams #-}

------------------------------------------------------------------------------------------------
-- |
-- Module      :  Pearl.GaDtTLHT.Internal.Unhom
-- Description :                          Unhoms
-- Copyright   :  (c) Drew Day 2012
--                (c) Shin-Cheng Mu 2011
--                (c) Akimasa Morihata 2011
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Drew Day <drewday@gmail.com>
-- Stability   :  stable
-- Portability :  portable (ViewPatterns, PatternGuards)
--
------------------------------------------------------------------------------------------------
module Pearl.GaDtTLHT.Internal.Unhom where

{-
hom :: [a] -> b
e :: b
k :: a -> b
f :: (b,b) -> b
-}

-- |
-- This is the a "correct" definition of hom, except in (at least) two senses:
-- 
-- 1. f is the leftmost biased choice (it splits (h [head], h [tail])
--
-- 2. in pointfree style, f, k, should be implicit.

hom :: ((b,b) -> b) -> (a -> b) -> b -> [a] -> b
hom f k e []     = e
hom f k e [x]    = k x
hom f k e (x:xs) = f (h [x], h xs)
               where h = hom f k e


unhom g f z s = k
              where 
               k v |          z v = []
                   |          s v = [f v]
                   | (l,r) <- g v = k l ++ k r



-- The "trivial examples" (along with their definitions) in Gibbons' paper are:
--
-- * id
--
-- @
--   id x = x
-- @
--
-- * map f
--
-- @ 
--   map _ [    ] = []
--   map f (x:xs) = f x : map f xs
-- @
--
-- * concat
--
-- @
--   concat = foldr (++) []
-- @
--
-- * (++)
--
-- @
-- [    ] ++ ys = ys
-- (x:xs) ++ ys = x : (xs ++ ys)
-- 
-- -- OR --
-- 
-- xs ++ ys = foldr (:) ys xs
-- @
--
-- * head
--
-- @
-- head (x:_) = x
-- head []    = undefined
-- @
--
-- * sum
-- @ sum = foldl (+) 0 @
--
-- * min
-- @ min x y = if x <= y then x else y @
--
-- * max
-- @ max x y = if x <= y then y else x @
--
-- * all
-- @ all p = and . map p @
--
-- * any
-- @ any p = or . map p @
--



sumH = hom f k e
          where
                f (l,r) = l + r
                k x     = x
                e       = 0

hsumL = foldr (+) 0 {- xs -}
hsumR = foldl (+) 0 {- xs -}



blah = undefined
test xs = (sumH xs, hsumL xs, hsumR xs)



{-
-- | extra (not in paper)
unhom_without_pattern_guards g f p q = k
     where 
     'k' v | p v = []
         | q v = [f v]
         | otherwise = case g v of (l,r) -> 'k' l ++ 'k' r

-- | extra (not in paper)
unhom3 g f p q = k
     where 
     'k' v | p v = []
         | q v = [f v]
         | (l,c,r) <- g v = 'k' l ++ 'k' c ++ 'k' r

-}

