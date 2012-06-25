{-# LANGUAGE ImplicitParams #-}

------------------------------------------------------------------------------------------------
-- |
-- Module      :  Pearl.GaDtTLHT.Internal.Hom
-- Description :                          Homs
-- Copyright   :  (c) Drew Day 2012
--                (c) Shin-Cheng Mu 2011
--                (c) Akimasa Morihata 2011
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Drew Day <drewday@gmail.com>
-- Stability   :  stable
-- Portability :  portable (ViewPatterns, PatternGuards)
--
------------------------------------------------------------------------------------------------
module Pearl.GaDtTLHT.Internal.Hom where

import Data.Maybe


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


-- The "trivial examples" (along with their definitions) in Gibbons' paper are:
--
-- * id
--
-- @
--   id x = x
-- @
idH = hom f k e
          where
                f (l,r) = id l ++ id r
                k x     = x
                e       = []

--idHr = foldr (id) []
--idHl = foldl (id) []


--
-- * map f
--
-- @ 
--   map _ [    ] = []
--   map f (x:xs) = f x : map f xs
-- @
--
--

lengthH = length' . map (\_ -> 1)
     where
          length' = sumH

lenH = hom f k e
          where
                f (l,r) = l
                k x     = 1
                e       = 0

-- lenHr = foldr (length) 0
-- lenHl = foldl (length) 0





-- * head
--
-- @
-- head (x:_) = x
-- head []    = undefined
-- @
--
headH = hom f k e
          where
                f (Just l, _) = Just l
                k x     = Just x
                e       = Nothing

headHr = foldr (head) Nothing
-- headHl = foldl (fst) []



-- * max
-- @ max x y = if x <= y then y else x @
maxH = hom f k e
          where
                f (l,r) = l `max` r
                k x     = x
                e       = minBound :: Int

maxHr = foldr (max) (minBound :: Int)
maxHl = foldl (max) (minBound :: Int)




-- * min
-- @ min x y = if x <= y then x else y @
minH = hom f k e
          where
                f (l,r) = l `min` r
                k x     = x
                e       = maxBound :: Int


minHr = foldr (min) (maxBound :: Int)
minHl = foldl (min) (maxBound :: Int)


-- * any
-- @ any p = or . map p @
--
anyH = hom f k e
          where
                f (l,r) = l || r
                k x     = x
                e       = False

anyHr = foldr (||) False
anyHl = foldl (||) False


-- * all
-- @ all p = and . map p @
--
allH = hom f k e
          where
                f (l,r) = (&&) l r
                k x     = x
                e       = True

allHr = foldr (&&) True
allHl = foldl (&&) True






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
catH = hom f k e
          where
                f (l,r) = l ++ r
                k x     = x
                e       = []

catHr = foldr (++) []
catHl = foldl (++) []


-- * sum
-- @ sum = foldl (+) 0 @
-- 
sumH = hom f k e
          where
                f (l,r) = l + r
                k x     = x
                e       = 0

sumHr = foldr (+) 0 {- xs -}
sumHl = foldl (+) 0 {- xs -}


sumHLR xs = (sumH, sumHr, sumHl)





