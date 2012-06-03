------------------------------------------------------------------------------------------------
-- |
-- Module      :  Pearl.GaDtTLHT.Section04
-- Description :  Generating Sequences from the Middle
-- Copyright   :  (c) Drew Day 2012
--                (c) Shin-Cheng Mu 2011
--                (c) Akimasa Morihata 2011
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Drew Day <drewday@gmail.com>
-- Stability   :  stable
-- Portability :  portable (ViewPatterns, PatternGuards)
--
------------------------------------------------------------------------------------------------
module Pearl.GaDtTLHT.Section04 
(
-- * Generating Sequences from the Middle

-- ** Quicksort <- Selection Sort
-- $qsortfromssort

-- *** Selection Sort
-- $ssort

-- *** g <- Lemma 06
-- $gfromlemma06

-- * \('p09'\)
-- $property09

-- ** Proof
-- $property09proof

-- *** A Valid Definition
-- $qsort


-- ** Parallel Scan

-- *** scanl

-- **** scan

-- *** reduce

-- *** growers

-- * \('p10'\)
-- $property10

-- ** Proof
-- $property10proof

note02 
) where

import Pearl.GaDtTLHT.Section03 (unfoldr,unfoldrp,unfoldl,unfoldlp)
import Pearl.GaDtTLHT.Ref

import Data.IntSet (IntSet)
import qualified Data.IntSet as Set

import Data.IntSet.Unicode ((∈),(∋),(∪),(∩),(∅))


test04 = id
-- ^
-- >>> 2 + 2
-- 4

-- $qsortfromssort
-- 
-- While Gibbons [@'r06'@] demonstrated how to derive merge sort from
-- insertion sort using the third list homomorphism theorem, we use
-- the dual theorem to derive quicksort from selection sort. [@'note02'@]
-- 
-- 
-- 
-- We regard the input as a set and denote disjoint union by `djU` .
-- 
-- One may come up with two definitions of selection sort: 
-- 
-- 

-- $ssort
-- 
-- @
-- sort = 'unfoldr' ('||>')  p 
--      = 'unfoldl' ('<||')  p
--      , where p = null and
-- @
-- 
-- @
-- ('||>')  zs | zs /= ∅ , x <- min zs = ( x, zs `without` { x }) ,
-- ('<||')  zs | zs /= ∅ , x <- max zs = ( zs `without` { x } , x ) .
-- @
-- 
-- 
-- Let 
-- 
-- @
--     xs ≤ ys ≡ (∀ x ∈ xs,
--                  y ∈ ys ∶∶ x ≤ y ).
-- @
-- 
-- One can see that
-- 
-- @
-- ( sort xs,      ys ) ∈ 'unfoldrp ('||>')  p zs,
-- (      xs, sort ys ) ∈ 'unfoldlp ('<||')  p zs,
-- 
-- if xs `djU` ys = zs and xs ≤ ys. 
-- @
-- 
-- We show only the proof of the first membership, for which we show that
-- 


sortR = unfoldr (||>)  p 
     where 
          p     = Set.null
          (||>) = undefined

sortL = unfoldl (<||)  p
     where
          p     = Set.null
          (<||) = undefined
          



-- $property09
-- 
-- @
--  ('p09')  ( ws ++ sort xs, ys ) ∈ iterF ( ws, zs ) <== xs `djU` ys = zs ∧ xs ≤ ys
-- @
-- 
-- 
-- Property ('p09') can be proved by induction on the size of zs. For
-- zs = ∅ the property trivially holds. The nonempty case is shown
-- in Figure 2.
-- 


-- $property09proof
-- @
-- ------- proof of (9)
-- ( ws ++ sort xs, ys ) ∈ iterF ( ws, zs )
-- 
-- ≡      { zs /= ∅ , let z = min zs }
-- 
-- ( ws ++ sort xs, ys ) = ( ws, zs ) ∨ ( ws ++ sort xs, ys ) ∈ iterF ( ws ++ [ z ] , zs − { z })
-- 
-- <==      { for non-empty xs, let x = min xs }
-- 
-- ( xs = ∅ ∧ ys = zs ) ∨ ( ws ++ [ x ] ++ sort ( xs − { x }) , ys ) ∈ iterF ( ws ++ [ z ] , zs − { z })
-- 
-- <==      { induction }
-- 
-- ( xs = ∅ ∧ ys = zs ) ∨ (( xs − { x } `djU` ys = zs − { z } ∧ xs ˙ ≤ ys ∧ x = z )
-- 
-- ≡ xs `djU` ys = zs ∧ xs  ≤ ys.
-- @
-- 
-- 


-- $gfromlemma06
-- 
-- By Lemma 6 we may thus choose 
-- 
-- @
-- g zs =  ( xs, ys ) for any
-- xs `djU` ys = zs ∧ xs ≤ ys. 
-- @
-- 
-- 
-- That is, we split the set zs into two, such
-- that all elements in one set are no larger than any element in the
-- other set, and sort them recursively. That gives rise to the equation,
-- 
-- @
-- sort ( xs `djU` ys ) | xs ≤ ys = sort xs ++ sort ys.
-- @
-- 
-- Despite being valid, the equation does not form a definition - as a 
-- program sort might not terminate since, for example, xs could be
-- empty and the size of ys equals that of zs. For this example, one may come up 
-- with a terminating definition by enforcing that the neither xs nor ys is empty. 
-- 
-- 


-- $qsort
-- 
-- We thus have 
-- 
-- @
-- sort = unhom g f p q
-- where g zs = ( xs, ys ) 
-- @
-- 
-- for some non-empty xs and ys such that
-- 
-- @
-- xs `djU` ys = zs ∧ xs ≤ ys, f { x } = x, p xs ≡ xs = ∅ , and q xs holds if
-- xs is singleton.
-- @
-- 
-- By /unfolding/ sort by one step we come up with
-- the definition:
-- 
-- @
-- sort ∅ = [ ]
-- sort ( xs `djU` { x } `djU` ys )  | xs  ≤ { x }  ≤ ys
-- = sort xs ++ [ x ] ++ sort ys.
-- @
-- 
-- 



-- $pscanintro
-- 
-- It is known that the Haskell prelude function 'scanl' (⊕) e, when
-- (⊕) is associative and e = ι⊕, the unit of (⊕) , is both a 'foldr' and a
-- 'foldl'. Geser and Gorlatch [@'r5'@] in fact showed how the following list
-- homomorphism can be derived using the third list homomorphism
-- theorem:
-- 
-- 
-- @
-- scanl (⊕) ι⊕ ( xs ++ ys ) 
--      | xs' ++ [ x ] <- scanl (⊕) ι⊕ xs
--      = xs' ++ [ x ] ++  map ( x ⊕) ( scanl (⊕) ι⊕ ys )
-- @
-- 
-- In an actual implementation, however, one would like to avoid having to perform map ( x ⊕) .
-- Here we demonstrate that an attention to unfolds leads to a faster program.
-- 
-- 
-- $pscanlossy
-- 
-- For a concise presentation we again consider a slightly different
-- variation. The following scan discards the right-most element of the input list:
-- 
-- @
-- scan ( e, [ ])      = [ ]
-- scan ( e, x ∶ xs ) = e ∶ scan ( e ⊕ x, xs )
-- @
-- 
-- 
-- 
-- $pscanexample
-- 
-- For example
-- 
-- @ scan ( e, [ 1, 2, 3 ]) = [ e, e ⊕ 1, e ⊕ 1 ⊕ 2 ] @
-- 
-- It is not hard to show that
-- 
-- @
-- scan ( e, xs ++ [ x ])  = scan ( e, xs ) ++ [ e ⊕ reduce xs ] ,
--      where 
--                reduce = hom (⊕) id ι⊕
-- @
-- 
-- 
-- We thus have:
-- 
-- @
-- scan = 'unfoldr' ('||>') p 
--      = 'unfoldl' ('<||') p
--   where p = null '.' snd 
-- @
-- 
-- and:
-- 
-- @
--                     ('||>') ( e, x ∶ xs )     = ( e, ( e ⊕ x, xs ))
--                     ('<||') ( e, xs ++ [ ]) = (( e, xs ) , e ⊕ reduce xs )
-- @
-- 
-- where the domains of ('||>') and ('<||') are pairs whose second components
-- are non-empty.
-- 
-- 
-- To construct g, we show that for xs ++ ys = zs we have:
-- 
-- @
-- ( scan ( e, xs ) ,      ( e ⊕ reduce xs, ys )) ∈ 'unfoldrp' ('||>')  p ( e, zs )
-- (      ( e, xs ) , scan ( e ⊕ reduce xs, ys )) ∈ 'unfoldlp' ('<||')  p ( e, zs )
-- @
-- 
-- 
-- Again we prove only the first property, for which we need to prove
-- a slight generalisation,
-- 
-- 

-- $property10
-- 
-- @
-- ( ws ++ scan ( e, xs ) , ( e ⊕ reduce xs, ys )) ∈ iterF ( ws, ( e, zs )) (10)
-- if xs ++ ys = zs.
-- @
-- 
-- The proof is an uninteresting induction on zs, and the inductive case is shown:


-- $property10proof
-- @
--    ( ws ++ scan ( e, xs ) , ( e ⊕ r xs, ys )) ∈ iterF ( ws, ( e, z ∶ zs ))
-- ≡  ( ws ++ scan ( e, xs ) , ( e ⊕ r xs, ys )) = ( ws, ( e, z ∶ zs )) ∨
--    ( ws ++ scan ( e, xs ) , ( e ⊕ r xs, ys )) ∈ iterF ( ws ++ [ e ] , ( e ⊕ z, zs ))
-- 
-- <==  { let xs = x ∶ xs' for the non-empty case }
-- 
-- ( xs = [ ] ∧ ys = z ∶ zs ) ∨ ( ws ++ [ e ] ++ scan ( e ⊕ x, xs') , ( e ⊕ x ⊕ r xs', ys )) ∈ iterF ( ws ++ [ e ] , ( e ⊕ z, zs ))
-- 
-- <==      { induction }
-- 
-- ( xs = [ ] ∧ ys = z ∶ zs ) ∨ ( xs' ++ ys = zs ∧ x = z )
-- 
-- ≡ xs ++ ys = z ∶ zs.
-- @
-- 
-- 
-- Thus we pick 
-- 
-- @
--      g ( e, zs )  =  (( e, xs ) , ( e ⊕ reduce xs, ys )) for some xs ++ ys = zs. 
-- @
-- 
-- For termination we want xs and ys to be both non-empty, which gives rise to the definition:
-- 
-- @
-- scan ( e, [ ]      )  = [ ]
-- scan ( e, [ x ]    )  = [ e ]
-- scan ( e, xs ++ ys )  |                                                    xs /= [ ] ∧ ys /= [ ]
--                       = scan ( e, xs ) ++ scan ( e ⊕ reduce xs, ys )
-- @
-- 
-- This is not yet an efficient implementation. To avoid repeated
-- calls to reduce, one typically performs a tupling. 
-- 
-- Let
-- 
-- @ 
-- sr ( e, xs ) = ( scan ( e, xs ) , reduce xs )
-- @
-- 
-- One may calculate a definition for sr:
-- 
-- @
-- sr ( e, [ ])        = ([ ] , ι⊕ )
-- sr ( e, [ x ])      = ([ e ] , x )
-- sr ( e, xs ++ ys )  |                                                       xs /= [ ] ∧ ys /= [ ]
--                     = let ( s1 , r1 ) = sr ( e, xs )
--                           ( s2 , r2 ) = sr ( e ⊕ r1 , ys )
--                        in ( s1 ++ s2 , r1 ⊕ r2 ) .
-- @
-- 
-- However, the second call to sr in the xs ++ ys case demands the
-- value of r1, which is a result of the first call to sr. 
-- This prevents the two calls to sr from being executed in parallel.
-- 
-- 
-- Instead, we compute scan in two phases: all the r’s are first
-- computed and cached, which are then used in the second phase to
-- compute scan. For that we need a data structure storing the r’s.
-- Consider the following binary tree, with a function val extracting
-- the value at the root,
-- 
-- @
-- data Tree a = L a | N ( Tree a ) a ( Tree a ) ,
-- 
-- val ( L   n    ) = n
-- val ( N _ n _  ) = n.
-- @
-- 
-- 
-- The following function builds a tree out of a non-empty list, with
-- the invariant that val ( build xs ) = reduce xs:
-- 
-- 
-- @
-- build [ x ]         = L x
-- build ( xs ++ ys )  |                                                      xs /= [ ] ∧ ys /= [ ]
--                     = let t = build xs
--                           u = build ys
--                       in 
--                          N t ( val t ⊕ val u ) u
-- @
-- 
-- The key to construct an efficient implementation of scan is to
-- perform build in a separate phase and use only the results stored in
-- the tree. That is, we wish to construct some function f such that
-- 
-- @
-- scan ( e, xs ) = f ( e, build xs ) .
-- @
-- 
-- The singleton case is easy: 
-- 
-- @
-- f ( e, L x ) = [ e ] . 
-- @
-- 
-- For inputs of length at least two, we calculate (for non-empty xs and ys):
-- 
-- @
-- scan ( e, xs ++ ys )
-- = scan ( e, xs ) ++ scan ( e ⊕ reduce xs, ys )
-- =      { since val ( build xs ) = reduce xs }
-- scan ( e, xs ) ++ scan ( e ⊕ val ( build xs ) , ys )
-- =      { induction: scan ( e, xs ) = f ( e, build xs )  }
-- f ( e, build xs ) ++ f ( e ⊕ val ( build xs ) , build ys )
-- =      { let N t v u = build ( xs ++ ys )  }
-- f ( e, t ) ++ f ( e ⊕ val t, u )
-- =      { let f ( e, N t   u ) = f ( e, t ) ++ f ( e ⊕ val t, u )  }
-- f ( e, build ( xs ++ ys )) .
-- @
-- 
-- We rename f to acc since it accumulates the result:
-- 
-- @
-- scan ( e, [ ])      = [ ]
-- scan ( e, xs )      = acc ( e, build xs ) ,
-- @
-- 
-- @
-- acc ( e, L x )      = [ e ]
-- acc ( e, N t   u ) = acc ( e, t ) ++ acc ( e ⊕ val t, u ) .
-- @
-- 
-- By constructing a balanced binary tree, the evaluation can be performed in O ( log n ) time 
-- given a sufficient number of processors, or in O ( n / p + log p ) time if the number of
-- processors is much smaller than n. We have in fact reconstructed a well-known efficient
-- implementation of scan recorded by, for example, Blelloch [@'r02'@].
-- 
-- 

note02 = undefined
-- ^
-- What we derive here, however, is the toy Quicksort well-known among
-- functional programmers. It is arguable that the essence of real Quicksort is
-- the algorithm for partition, which is not addressed here.

