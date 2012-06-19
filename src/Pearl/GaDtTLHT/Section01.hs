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
       p00
     -- *** Foldable, and The Third-List Homomorphism Theorem
          -- $thdlist
     , e00

     -- *** A Question of Associative Operators
          -- $question01

     -- *** Is there a dual theorem, Unfoldable?
          -- $observation

     -- *** \
          -- $question02

     -- *** \  
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

     -- * Familar Functions
     -- $familiar
     
     -- ** (<- Prelude?) <- Data.List
     -- $hasklistsigs


     -- * Unfamiliar Functions
     -- $unfamiliar

     -- ** \<- Data.DList
     -- $difflistsigs

     -- * Comparing "Data.List" and "Data.Dlist"
     -- $comparefoldsigs
     , h
     , (⊙)
     )         where
import Pearl.GaDtTLHT.References

import qualified Data.List  as HaskList
import qualified Data.DList as DList

-- $fromprelude
--
-- These functions are part of the Haskell Prelude, so they are sitting at your fingertips, like it or not!

-- $familiar
--
-- This functional pearl assumes familiarity and understanding  of the following functions, as variants of these will be discussed.
--

-- $unfamiliar
--
-- This functional pearl assumes a passable understanding of the following functions





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


p00 :: property (h is a list homomorphism)
p00 = undefined
-- ^
-- @
--   ('p00')      'h' (xs '++' ys) = 'h' xs ⊙ 'h' ys
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

e00 :: example (sum with plus and zero) over (lists with append) (is both a foldl and a foldr) [thlt]
e00 = undefined
-- ^
-- For example, since:
-- 
-- @
--      'sum' 
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
--


-- $hasklistsigs
--
-- Here is the /huge/ list of functions exported by "Data.List". 
--
-- Don't worry, though! You only need to know about those which are linked. 
-- Many of these are exported to the "Prelude" already, so you should be familiar with them. Right?
--
-- @
-- \  -- * Basic functions
-- \  
-- \    ('++')              -- :: [a] -> [a] -> [a]
-- \  , head              -- :: [a] -> a
-- \  , last              -- :: [a] -> a
-- \  , tail              -- :: [a] -> [a]
-- \  , init              -- :: [a] -> [a]
-- \  , 'null'              -- :: [a] -> Bool
-- \  , length            -- :: [a] -> Int
-- \  
-- \  -- * List transformations
-- \  , 'map'               -- :: (a -> b) -> [a] -> [b]
-- \  , reverse           -- :: [a] -> [a]
-- \  
-- \  , intersperse       -- :: a -> [a] -> [a]
-- \  , intercalate       -- :: [a] -> [[a]] -> [a]
-- \  , transpose         -- :: [[a]] -> [[a]]
-- \  
-- \  , 'subsequences'      -- :: [a] -> [[a]]
-- \  , permutations      -- :: [a] -> [[a]]
-- \  
-- \  -- * Reducing lists (folds)
-- \  
-- \  , 'foldl'             -- :: (a -> b -> a) -> a -> [b] -> a
-- \  , 'foldl''            -- :: (a -> b -> a) -> a -> [b] -> a
-- \  , 'foldl1'            -- :: (a -> a -> a) -> [a] -> a
-- \  , 'foldl1''           -- :: (a -> a -> a) -> [a] -> a
-- \  , 'foldr'             -- :: (a -> b -> b) -> b -> [a] -> b
-- \  , 'foldr1'            -- :: (a -> a -> a) -> [a] -> a
-- \  
-- \  -- ** Special folds
-- \  
-- \  , 'concat'            -- :: [[a]] -> [a]
-- \  , 'concatMap'         -- :: (a -> [b]) -> [a] -> [b]
-- \  , and               -- :: [Bool] -> Bool
-- \  , or                -- :: [Bool] -> Bool
-- \  , any               -- :: (a -> Bool) -> [a] -> Bool
-- \  , all               -- :: (a -> Bool) -> [a] -> Bool
-- \  , 'sum'               -- :: (Num a) => [a] -> a
-- \  , product           -- :: (Num a) => [a] -> a
-- \  , 'maximum'           -- :: (Ord a) => [a] -> a
-- \  , 'minimum'           -- :: (Ord a) => [a] -> a
-- \  
-- \  -- * Building lists
-- \  
-- \  -- ** Scans
-- \  , 'scanl'             -- :: (a -> b -> a) -> a -> [b] -> [a]
-- \  , scanl1            -- :: (a -> a -> a) -> [a] -> [a]
-- \  , 'scanr'             -- :: (a -> b -> b) -> b -> [a] -> [b]
-- \  , scanr1            -- :: (a -> a -> a) -> [a] -> [a]
-- \  
-- \  -- ** Accumulating maps
-- \  , mapAccumL         -- :: (a -> b -> (a,c)) -> a -> [b] -> (a,[c])
-- \  , mapAccumR         -- :: (a -> b -> (a,c)) -> a -> [b] -> (a,[c])
-- \  
-- \  -- ** Infinite lists
-- \  , iterate           -- :: (a -> a) -> a -> [a]
-- \  , repeat            -- :: a -> [a]
-- \  , replicate         -- :: Int -> a -> [a]
-- \  , cycle             -- :: [a] -> [a]
-- \  
-- \  -- ** Unfolding
-- \  , 'unfoldr'           -- :: (b -> Maybe (a, b)) -> b -> [a]
-- \  
-- \  -- * Sublists
-- \  
-- \  -- ** Extracting sublists
-- \  , take              -- :: Int -> [a] -> [a]
-- \  , drop              -- :: Int -> [a] -> [a]
-- \  , splitAt           -- :: Int -> [a] -> ([a], [a])
-- \  
-- \  , takeWhile         -- :: (a -> Bool) -> [a] -> [a]
-- \  , dropWhile         -- :: (a -> Bool) -> [a] -> [a]
-- \  , dropWhileEnd      -- :: (a -> Bool) -> [a] -> [a]
-- \  , span              -- :: (a -> Bool) -> [a] -> ([a], [a])
-- \  , break             -- :: (a -> Bool) -> [a] -> ([a], [a])
-- \  
-- \  , stripPrefix       -- :: Eq a => [a] -> [a] -> Maybe [a]
-- \  
-- \  , group             -- :: Eq a => [a] -> [[a]]
-- \  
-- \  , inits             -- :: [a] -> [[a]]
-- \  , tails             -- :: [a] -> [[a]]
-- \  
-- \  -- ** Predicates
-- \  , 'isPrefixOf'        -- :: (Eq a) => [a] -> [a] -> Bool
-- \  , 'isSuffixOf'        -- :: (Eq a) => [a] -> [a] -> Bool
-- \  , isInfixOf         -- :: (Eq a) => [a] -> [a] -> Bool
-- \  
-- \  -- * Searching lists
-- \  
-- \  -- ** Searching by equality
-- \  , 'elem'              -- :: a -> [a] -> Bool
-- \  , 'notElem'           -- :: a -> [a] -> Bool
-- \  , 'lookup'            -- :: (Eq a) => a -> [(a,b)] -> Maybe b
-- \  
-- \  -- ** Searching with a predicate
-- \  , find              -- :: (a -> Bool) -> [a] -> Maybe a
-- \  , filter            -- :: (a -> Bool) -> [a] -> [a]
-- \  , partition         -- :: (a -> Bool) -> [a] -> ([a], [a])
-- \  
-- \  
-- \  -- ** \"Set\" operations
-- \  
-- \  , nub               -- :: (Eq a) => [a] -> [a]
-- \  
-- \  , delete            -- :: (Eq a) => a -> [a] -> [a]
-- \  , (\\\\)              -- :: (Eq a) => [a] -> [a] -> [a]
-- \  
-- \  , 'union'             -- :: (Eq a) => [a] -> [a] -> [a]
-- \  , 'intersect'         -- :: (Eq a) => [a] -> [a] -> [a]
-- \  
-- \  -- ** Ordered lists
-- \  , 'sort'              -- :: (Ord a) => [a] -> [a]
-- \  , 'insert'            -- :: (Ord a) => a -> [a] -> [a]
-- \  
-- \  -- * Generalized functions
-- \  
-- \  -- ** The By operations
-- \  -- | By convention, overloaded functions have a non-overloaded
-- \  -- counterpart whose name is suffixed with \`By\'.
-- \  --
-- \  -- It is often convenient to use these functions together with
-- \  -- \'on\', for instance \'sortBy\' (\'compare\' \`on\` \'fst\').
-- \  
-- \  -- *** User-supplied equality (replacing an Eq context)
-- \  -- | The predicate is assumed to define an equivalence.
-- \  , nubBy             -- :: (a -> a -> Bool) -> [a] -> [a]
-- \  , deleteBy          -- :: (a -> a -> Bool) -> a -> [a] -> [a]
-- \  , deleteFirstsBy    -- :: (a -> a -> Bool) -> [a] -> [a] -> [a]
-- \  , unionBy           -- :: (a -> a -> Bool) -> [a] -> [a] -> [a]
-- \  , intersectBy       -- :: (a -> a -> Bool) -> [a] -> [a] -> [a]
-- \  , groupBy           -- :: (a -> a -> Bool) -> [a] -> [[a]]
-- \  
-- \  -- *** User-supplied comparison (replacing an Ord context)
-- \  -- | The function is assumed to define a total ordering.
-- \  , sortBy            -- :: (a -> a -> Ordering) -> [a] -> [a]
-- \  , insertBy          -- :: (a -> a -> Ordering) -> a -> [a] -> [a]
-- \  , maximumBy         -- :: (a -> a -> Ordering) -> [a] -> a
-- \  , minimumBy         -- :: (a -> a -> Ordering) -> [a] -> a
-- \  
-- \  -- ** The \"generic\" operations
-- \  -- | The prefix \`generic\' indicates an overloaded function that
-- \  -- is a generalized version of a \"Prelude\" function.
-- \  
-- \  , genericLength     -- :: (Integral a) => [b] -> a
-- \  , genericTake       -- :: (Integral a) => a -> [b] -> [b]
-- \  , genericDrop       -- :: (Integral a) => a -> [b] -> [b]
-- \  , genericSplitAt    -- :: (Integral a) => a -> [b] -> ([b], [b])
-- \  , genericIndex      -- :: (Integral a) => [b] -> a -> b
-- \  , genericReplicate  -- :: (Integral a) => a -> b -> [b]
-- @

-- $difflistsigs
--
-- The export list for DList is incredibly small compared to that of 'Data.List'. Again,
-- the linked functions will be those relevant to this pearl.
--
-- @
-- \  -- * Construction
-- \  , 'DList.fromList'      -- :: [a] -> DList a
-- \  , 'DList.toList'        -- :: DList a -> [a]
-- \  -- * Basic functions
-- \  , 'DList.empty'         -- :: DList a
-- \  , 'DList.singleton'     -- :: a -> DList a
-- \  , 'DList.cons'          -- :: a -> DList a -> DList a
-- \  , 'DList.snoc'          -- :: DList a -> a -> DList a
-- \  , 'DList.append'        -- :: DList a -> DList a -> DList a
-- \  , 'DList.concat'        -- :: [DList a] -> DList a
-- \  , replicate     -- :: Int -> a -> DList a
-- \  , list          -- :: b -> (a -> DList a -> b) -> DList a -> b
-- \  , head          -- :: DList a -> a
-- \  , tail          -- :: DList a -> DList a
-- \  , 'DList.unfoldr'       -- :: (b -> Maybe (a,  b)) -> b -> DList a
-- \  , 'DList.foldr'         -- :: (a -> b -> b) -> b -> DList a -> b
-- \  , 'map'           -- :: (a -> b) -> DList a -> DList b
-- \  -- * MonadPlus
-- \  ,  maybeReturn
-- @
-- 


-- $comparefoldsigs
-- 
-- "Data.List":
--
-- @
-- \  , 'foldr'             -- :: (a -> b -> b) -> b -> [a] -> b
-- \  , 'Data.List.unfoldr'           -- :: (b -> Maybe (a, b)) -> b -> [a]
-- \ 
-- \  , 'map'               -- :: (a -> b) -> [a] -> [b]
-- \ 
-- \  , 'concat'            -- :: [[a]] -> [a]
-- \  , 'concatMap'         -- :: (a -> [b]) -> [a] -> [b]
-- @
--
-- "Data.DList":
--
-- @
--   , 'DList.foldr'         -- :: (a -> b -> b) -> b -> DList a -> b
--   , 'DList.unfoldr'       -- :: (b -> Maybe (a,  b)) -> b -> DList a
-- \
--   , 'map'           -- :: (a -> b) -> DList a -> DList b
-- \  
--   , 'DList.append'        -- :: DList a -> DList a -> DList a
--   , 'DList.concat'        -- :: [DList a] -> DList a
-- @


-- @
--  \  , 'foldl'             -- :: (a -> b -> a) -> a -> [b] -> a
--  \  , 'foldr1'            -- :: (a -> a -> a) -> [a] -> a
--  \  , 'foldl''            -- :: (a -> b -> a) -> a -> [b] -> a
--  \  , 'foldl1'            -- :: (a -> a -> a) -> [a] -> a
--  \  , 'foldl1''           -- :: (a -> a -> a) -> [a] -> a
--  \  
--  \  , and               -- :: [Bool] -> Bool
--  \  , or                -- :: [Bool] -> Bool
--  \  , any               -- :: (a -> Bool) -> [a] -> Bool
--  \  , all               -- :: (a -> Bool) -> [a] -> Bool
--  \  , 'sum'               -- :: (Num a) => [a] -> a
--  \  , product           -- :: (Num a) => [a] -> a
--  \  , 'maximum'           -- :: (Ord a) => [a] -> a
--  \  , 'minimum'           -- :: (Ord a) => [a] -> a
--  \  
--  \  -- * Building lists
--  \  
--  \  -- ** Scans
--  \  , 'scanl'             -- :: (a -> b -> a) -> a -> [b] -> [a]
--  \  , scanl1            -- :: (a -> a -> a) -> [a] -> [a]
--  \  , 'scanr'             -- :: (a -> b -> b) -> b -> [a] -> [b]
--  \  , scanr1            -- :: (a -> a -> a) -> [a] -> [a]
--  \  
--  \  -- ** Accumulating maps
--  \  , mapAccumL         -- :: (a -> b -> (a,c)) -> a -> [b] -> (a,[c])
--  \  , mapAccumR         -- :: (a -> b -> (a,c)) -> a -> [b] -> (a,[c])
--  \  
--  \  -- ** Infinite lists
--  \  , iterate           -- :: (a -> a) -> a -> [a]
--  \  , repeat            -- :: a -> [a]
--  \  , replicate         -- :: Int -> a -> [a]
--  \  , cycle             -- :: [a] -> [a]
--  \  
--  \  -- ** Unfolding
--  \
--  @


