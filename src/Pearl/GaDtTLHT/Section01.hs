{-# LANGUAGE ViewPatterns, PatternGuards #-}
------------------------------------------------------------------------------------------------
-- |
-- Module      :  Pearl.GaDtTLHT.Section03
-- Description :  Introduction
-- Copyright   :  (c) Drew Day 2012
--                (c) Shin-Cheng Mu 2011
--                (c) Akimasa Morihata 2011
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Drew Day <drewday@gmail.com>
-- Stability   :  stable
-- Portability :  portable (ViewPatterns, PatternGuards)
------------------------------------------------------------------------------------------------

module Pearl.GaDtTLHT.Section01
     (
     -- ** Introduction 
          -- $intro
     
     -- *** Parallel Skeletons
          -- $skeletons
     h
     -- *** Foldable, and The Third-List Homomorphism Theorem
          -- $thdlist

     -- *** A Question of Associators     
          -- $question01

     -- *** Is there a Dual Theorem for Unfoldable?
          -- $observation

     -- *** Yes!
          -- $result01

     -- * Contents
          -- $contents

     -- ** Editor's Note

     -- *** Uncurrying and Point-Free Syntax
          -- $uncurry

          -- $cons
     , cons
     , cons_from_Prelude

          -- $concat
     , cat_from_Prelude
     , cat
     )         where
import Pearl.GaDtTLHT.Ref


(⊙) :: [a] -> [a] -> [a]
(⊙) = undefined
-- ^
-- An associative operator.
--


h :: [a] -> b
h = undefined
-- ^
-- >>> :t h []
-- h [] :: b


-- $intro
-- As multicore hardware has become standard in recent years, parallel programming rekindles as an important potential
-- application of functional programming. The skeletal parallel programming [@'r03'@] paradigm proposes the idea of
-- developing parallel programs by combining parallel skeletons - functions that capture useful parallel programming
-- patterns. 
--

-- $skeletons
-- Among the important parallel skeletons is list homomorphism [@'r01'@], one that satisﬁes the equation:
--
-- @
--        'h' (xs '++' ys) = 'h' xs ⊙ 'h' ys
-- @
--
-- which says that to compute 'h':
--
-- 1. one may arbitrarily split the input list into xs '++' ys,
--
-- 2. compute 'h' on them recursively in parallel, 
--
-- 3. and combine the results using an associative operator @⊙@.


-- $thdlist
-- A well-known third list-homomorphism theorem [@'r03'@] says that a function is a list homomorphism if it can be described
-- as an instance of both 'foldr' and 'foldl'. 
--
-- For example, since:
-- 
-- @
--      sum 
--           = 'foldr' ('+') 0 
--           = 'foldl' ('+') 0
-- @
--
-- Then there exists some ⊙ such that:
-- 
-- @
--      'sum' ( xs '++' ys )  = 'sum' xs ⊙ 'sum' ys
-- @       
--
-- For this simple example, (⊙) happens to be ('+') as well. 


-- $question01
-- One naturally wonders whether (⊙) can be mechanically constructed.
--
-- Such methods have been proposed (
--        [@'r05'@], 
--        [@'r09'@], 
--  and   [@'r11'@] )
--  and even generalised to trees: [@'r10'@].
--

-- $observation 
-- Less noticed, however, is that the theorem and its proof dualise very well to unfolds on lists. 
-- 
-- Consider the function 
-- 
-- @
--      fromTo ( x, y ) = [ x, x + 1 ... y ].
-- @
-- 
-- One may imagine three possible implementations:
-- 
-- 1. generating the list from the left
-- 
-- 2. generating the list from the right
-- 
-- 3. generating the list from some arbitrary point in the middle. 

-- $question02
-- Is it true that any function that can be defined as both an 'Data.Sequence.unfoldr' and an 'Data.Sequence.unfoldl' can be written as one that generates the list from the middle?

-- $result01
-- We show in this pearl that the answer is @positive@. This is not only of theoretical interest but could also have a practical impact. First, there are several efficient algorithms that are based on divide-and-conquer sequence generation, such as Quicksort. Moreover, the performance bottleneck in distributed parallel computing often lies in data distribution. Being able to generate the list anywhere allows us to distribute seeds of sublists and simultaneously generate from them, and thereby reduce communication costs and increase parallelism.

-- $contents
-- List homomorphisms and the third list-homomorphism theorem are reviewed in Section 2, before we present a dualised
-- theorem in Section 3 and apply it, in Section 4, to examples including sorting and parallel scan. In Section 5, the
-- results are further generalised to trees, before we conclude in Section 6.


-- $editornote
-- NOTE:
--
-- The rest of the documentation in this section (including 'cons_from_Prelude', 'cons', 'cat_from_Prelude', 'cat', ...) are provided
-- to demonstrate and witness the conversion from traditional point-full Haskell syntax to point-free Haskell syntax. This
-- should not be strictly necessary, but some programs are easier to express in one form or another. These examples were
-- motivated by, but not included in, the original paper ('r00').
--







-- $uncurry
--
-- Normal (or point-full) Haskell uses 'curry'-ed functions and function application ('ap', but really just
-- space) to programs.
--
-- In constrast to this, point-free Haskell syntax often uses 'uncurry'-ed functions and function composition ('.')
-- to describe programs.
--
-- Some examples of both styles, and examples on converting between them, follow.



-- $cons
cons_from_Prelude = (:)
-- ^
-- First, we uncurry the normal (curried) Haskell list constructor (':'), (which I've simply named @cons_from_Prelude@):

-- ^
-- >>> 1 : [2,3]
-- [1,2,3]

-- ^
-- >>> 1 `cons_from_Prelude` [2,3]
-- [1,2,3]

-- ^
-- >>> :t (:)
-- (:) :: a -> [a] -> [a]

-- ^
-- >>> :t (cons_from_Prelude)
-- (cons_from_Prelude) :: a -> [a] -> [a]

-- ^
-- >>> :t uncurry (cons_from_Prelude)
-- uncurry (cons_from_Prelude) :: (a, [a]) -> [a]


-- ^
-- What we want to get is 'cons':
--
cons :: (a, [a]) -> [a]
cons (x,xs)     = x:xs

-- ^
-- >>> cons (1,[2,3])
-- [1,2,3]

-- ^
-- >>> cons (1,[])
-- [1]

-- ^
-- >>> cons ([],[])
-- [[]]


-- $concat
cat_from_Prelude = (++)
-- ^
-- Then, we'll also need to uncurry ('++'), which I've named "cat_from_Prelude":

-- ^
-- >>> "this" ++ "word"
-- "thisword"

-- ^
-- >>> "this" `cat_from_Prelude` "word"
-- "thisword"

-- ^
-- This has a familar type:
--

{- TODO: Fix this. Doctest doesn't support unicode!
-- >>> :t ⊙
-- (⊙) :: [a] -> [a] -> [a]                            -}

-- ^
-- >>> :t cat_from_Prelude
-- cat_from_Prelude :: [a] -> [a] -> [a]

-- ^
-- >>> :t uncurry cat_from_Prelude
-- uncurry cat_from_Prelude :: ([a], [a]) -> [a]


-- ^
-- In the end, we get 'cat':
--
cat :: ([a], [a]) -> [a]
cat (xs,ys) = xs ++ ys
-- ^
-- >>> :t cat
-- cat :: ([a], [a]) -> [a]

-- ^
-- >>> cat ("this","word")
-- "thisword"

-- ^
-- >>> cat ([1,2,3],[4,5,6])
-- [1,2,3,4,5,6]

-- ^
-- >>> cat ([1,2,3],[])
-- [1,2,3]

-- ^
-- >>> cat ([],[1,2,3])
-- [1,2,3]















{-
uncurry1 :: (a -> b)           -> (a)     -> b
uncurry2 :: (a -> b -> c)      -> (a,b)   -> c
uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d

uncurry1 f (x) = f x
uncurry2 f (x,y) = f x y
uncurry3 f (x,y,z) = f x y z
-}
