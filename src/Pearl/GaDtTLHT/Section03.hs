{-# LANGUAGE ViewPatterns, PatternGuards #-}
------------------------------------------------------------------------------------------------
-- |
-- Module      :  Pearl.GaDtTLHT.Section03
-- Copyright   :  (c) Drew Day 2012
--                (c) Shin-Cheng Mu 2011
--                (c) Akimasa Morihata 2011
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Drew Day <drewday@gmail.com>
-- Stability   :  stable
-- Portability :  portable (ViewPatterns, PatternGuards)
--
------------------------------------------------------------------------------------------------
module Pearl.GaDtTLHT.Section03 
     (
     -- ** Unfolds! Left and Right!
          -- $unfoldr
       unfoldr
          -- $unfoldl
     , unfoldl
     -- ** Unfolds are (usually) coinductive...
          -- $inductive

     -- ** A definition for unhom
          -- $unhom
     , unhom
     -- ** Lemma 3
     , lemma03
     -- *** Proof: by Example
     , lemma03proof
     -- **** Example
     , example0
     -- ** Theorem 02 for Unfolds?
     , question
     , unfoldrp
     , unfoldlp
     -- ** /Traces/ of Unfolding
          -- $traces

     -- ** A crucial property
          -- $property
     , splits

     -- ** A Relational Perspective
          -- $relationcomments

     -- *** Relations: Introduction
          -- $relation

     -- **** Composition
          -- $relationcompose

     -- **** Converse
          -- $relationconverse

     -- **** Laws
          -- $relationlaws

     -- ** Theorem 4
          -- $theorem04

     -- *** Proof
          -- $theorem04proof

     -- ** Corollary 5 \<- Theorem 4
          -- $theorem05

     -- ** Proof
          -- $theorem05proof

     -- ** Calculating 'g'
          -- $calculating

     -- ** Figure 1
          -- $figure01

     -- *** Lemma 6
          -- $theorem06
     
     -- ** Extra Stuff
     , (<||)
     , (||>)
     , (|<|)
     , (|>|)
     , p
     , f
     , g
     , q
     , k
     , unfl
     , unfr
     , (/\)
     )    where

import Pearl.GaDtTLHT.Section02 ((><), foldlr, foldrr,cat,h)
import Pearl.GaDtTLHT.Ref
import Data.List.Unicode ((∈),(∋),(∪),(∩))

(<||) :: (b, t) -> t
(<||) (x, v) = f (k x,   v)
     where k :: b -> [a]
           k = undefined
           f = undefined

(||>) :: (t1, b) -> t
(||>) (v, x) = f (  v, k x)
     where
          k :: b -> [a]
          k = undefined
          f = undefined


-- $unfoldr
-- The function 'unfoldr', a dual of 'foldr' that generates a list from left
-- to right, may be defined as follows: ['note01']
-- 
-- @
-- 'unfoldr' ('||>')  'p' v
-- \       \  \   \ | 'p' v                  = []
-- \       \  \   \ | (x, v') \<- ('||>') v   = x : 'unfoldr' '(||>') 'p' v'
-- @
--
unfoldr :: (b -> (a,b)) -> (b -> Bool) -> b -> [a]
unfoldr (||>)    p v 
               | p v                    = []
               | (x, v') <- (||>) v     = x : unfoldr (||>) p v'

-- $unfoldl
-- Symmetrically, the function unfoldl is defined by:
-- 
-- @
-- 'unfoldl' ('<||')  'p'    v
-- \       \  \   \ | 'p'    v               = []
-- \       \  \   \ | (v', x) \<- ('<||') v   = 'unfoldl' ('<||') 'p' v' '++' [x]
-- @
unfoldl :: (b -> (b,a)) -> (b -> Bool) -> b -> [a]
unfoldl (<||)    p v
               | p v                    = []
               | (v', x) <- (<||) v     = unfoldl (<||) p v' ++ [x]


-- $inductive
-- Typically, unfolds are defined for /coinductive, possibly infinite lists/. Since we want the unfolded lists to have both a left end
-- and a right end, and for another important technical reason to be mentioned later, our "unfolds" in this pearl return inductive, finite
-- lists and require separate proofs that all successive applications of '<||' and '||>' eventually reach some @v@ for which @p v@ is 'True'.
-- Due to space constraints, however, the proof of termination is usually treated informally.
-- 

-- $unhom
-- Finally, we denote a function @k ∶∶ b → [ a ]@ by @'unhom' 'g' 'f' 'p' 'q'@ it it satisfies:
-- 
-- @
-- 'unhom' 'g' 'f' 'p' 'q' = 'k'
--      where 
--      'k' v | 'p' v          = []
--      \ \   | 'q' v          = ['f' v]
--      \ \   | (l,r) <- 'g' v = 'k' l '++' 'k' r
-- @
-- 
-- 
unhom g f p q = k
     where 
     k v | p v = []
         | q v = [f v]
         | (l,r) <- g v = k l ++ k r
-- ^
-- We also demand that successive applications of g eventually produce seeds for which either p or q is true, and thus 'k' generates finite
-- lists.
--
-- Regrettably, @Theorem 1@ does not dualise to unfolds.


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
-- ** Lemma 3
lemma03 :: a
lemma03 = undefined
-- ^
-- There /exist/ @h@ such that:
--
-- @ h = 'unhom' 'g' 'f' 'p' 'q' @
--
-- for which there is no corresponding:
--
-- @
--        h = 'unfoldr' ('||>') 'p'
--          = 'unfoldl' ('<||') 'p'
-- @
--

-- *** Proof (by example)
lemma03proof :: a
lemma03proof = undefined
-- ^
-- Let @'example0' :: Int -> [Int]@ generate a list of @1@s of length @2^n@ for a given n.
--
-- It can be defined:
--
-- @
--   example0 :: Int -> [Int]
--   example0 = unhom
--                  (\\n -> (n - 1, n - 1))   {-g-}
--                  (\\0 -> 1)                {-f-}
--                  (0 >)                    {-p-}
--                  (0 ==)                   {-q-}
-- @
--
-- But it /can not/ be defined by an 'unfoldl' or 'unfoldr'. An intuitive reason is that there
-- is not always an m such that @2^n - 1 = 2^m@. One may use theories of Gibbons et al. [@'ref07'@]
-- for proof.

example0 :: Int -> [Int]
example0 = unhom
               (\n -> (n - 1, n - 1))
               (\0 -> 1)
               (0 >)
               (0 ==)
-- ^
-- >>> map example0 [1..4]
-- [[1,1],[1,1,1,1],[1,1,1,1,1,1,1,1],[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]]

-- ^
-- >>> map (length . example0) [1..10]
-- [2,4,8,16,32,64,128,256,512,1024]


{-
-- | example of fibionacci-sized lists (not in paper)
example_fib :: Int -> [Int]
example_fib = unhom
               (\n -> (n - 1, n - 2))
               (\0 -> 1)
               (0 >)
               (0 ==)
-- | example of something else (not in paper)
example3 = unhom3
               (\n -> (n - 2, n - 2, n - 2))
               (\0 -> 1)
               (0 >)
               (0 ==)
-- ^
-- >>> map example3 [1,3..99]
-- [[],[],[],[],[],[],[],[],[],[],[],[],[],Interrupted. SLOW!

-}



question :: a
question = undefined
-- ^
-- Does theorem 2 have an unfold counterpart? 
--
-- To establish that:
--
-- @
--   'k' = 'unhom' 'g' 'f' p' 'q'
-- @
--
-- Given:
--
-- @
--   'k' = 'unfoldr' ('||>') 'p'
--   \ \ = 'unfoldl' ('<||') 'p'
-- @
--
-- We may simply pick:
--
-- @
-- 'k' = 'unhom' g \ f  \ p' \ q
-- \ \   \     \    \|   \|   \|
-- \ \   \     \    'f'   |   \|      =     'fst' . ('||>')        -- fixme?
-- \ \   \     \    \ \   p'  \|      = 'p'
-- \ \   \     \    \ \       'q'      = 'p' . 'snd' . ('<||')        -- fixme?
-- @
--
-- The challenge is to construct 'g'. To do so, we introduce some concepts dual to those for folds.
--
-- While 'foldrr' and 'foldlr' /resume/ a /partially computed/ fold, their duals 
--       'unfoldrp' and 'unfoldlp' instead /pause/ during the generation of lists. 
--
-- During the generation of a list, there are many places where we may pause.
-- The following functions 'unfoldrp' and 'unfoldlp' (@p@ for /pause/) return, in a list,
-- all intermediate lists and seeds during list generation:
--
-- @
-- 'unfoldr'p ('||>') 'p' v = ('|>|') ([], v)
--                where ('|>|') (xs, v) | 'p' v                = [(xs, v)]
--                                    | (x, v') \<- ('||>') v = (xs, v) : ('|>|') (xs '++' [x], v')
-- @
--
-- @
-- 'unfoldlp' ('<||') 'p' v = ('|<|') (v, []) 
--                where ('|<|') (v, xs) | 'p' v                 = [(v,xs)]
--                                    | (v', x) \<- ('<||') v  = ('|<|') (v', x:xs) '++' [(v,xs)]
-- @
--


unfoldrp :: (b -> (a,b)) -> (b -> Bool) -> b -> [([a], b)]
unfoldrp (||>) p v = (|>|) ([], v) where (|>|) (xs, v) | p v                = [(xs, v)]
                                                       | (x, v') <- (||>) v = (xs, v) : (|>|) (xs ++ [x], v')

unfoldlp :: (b -> (b,a)) -> (b -> Bool) -> b -> [(b, [a])]
unfoldlp (<||) p v = (|<|) (v, []) where (|<|) (v, xs) | p v                = [(v,xs)]
                                                       | (v', x) <- (<||) v = (|<|) (v', x:xs) ++ [(v,xs)]


-- $traces
--
-- We may think of 'unfoldrp' and 'unfoldlp' as returning the /traces/ of unfolding.
--
-- For example:
--
-- @
--   'unfoldrp' ('||>')  'p'  v0
-- @
--
-- Yields the list:
-- 
-- @
--   [  ([], v0) , ( [x1], v1 ) , ( [x1,x2] , v2 ), ( [x1,x2,x3], v3 ) ... ]
-- @
--
-- If:
--
-- @
--    ( x_i+1 , v_i+1 ) = ('||>') v_i
-- @
--


-- $property
--
-- A crucial property relating 'unfoldr' and 'unfoldrp' is that if:
--
-- @
--   'k' = 'unfoldr' ('||>')  'p'
--   'k' = 'unfoldl' ('<||')  'p'
-- @
--
-- we have both:
--
-- @
--   (5)       'splits' . 'k' = 'map' ('id' '><' k) . 'unfoldrp' ('||>')
--   (6)       'splits' . 'k' = 'map' (k '><' 'id') . 'unfoldlp' ('<||')
-- @
--
-- where
--
-- @
--   'splits' :: [a] -> ( [a], [a] )
-- @
--
-- Notice how they resemble the /converses/ of @(3)@ and @(4)@:
--
-- @
--   (3)       'h' '.' 'cat' = 'foldrr' ('<||') '.' ( 'id' '><' 'h' )
--   (4)       'h' '.' 'cat' = 'foldlr' ('||>') '.' ( 'h' '><' 'id' )
-- @
--
--
-- * the functions next to the composition ('.') are swapped
--
-- * instead of @f . cat@ we have @splits . k@ on the left hand sides
--
-- * @('id' '><' k)@ in @(5)@ is on the left-hand side of ('.'), and is lifted to lists by 'map'
--   due to the type, etc.
--

splits :: [a] -> ([a],[a])
splits = undefined
-- ^
-- >>> splits [1]
-- [ ([] , [1]) , ( [1], [] ) ]

-- ^
-- >>> splits [1,2]
-- [ ([], [1,2]), ([1], [2]), ([1,2], []) ]






-- $relationcomments
-- 
-- We could have proceeded from @(5)@ and @(6)@ to construct @g@ and thereby prove
-- the dual theorem. However, we would like to take a bold approach and ask the readers
-- to see paused unfolding /as a relation/.
--
-- Functional progammers appear to regard relations as an arcane creation, which is an
-- unfortunate misunderstanding.
--
-- Dijkstra argued that, for /program derivation/, non-determinism should be the norm
-- and determinism a special case ['ref04'], and this pearl is indeed a case where one
-- has to bring up relations to appreciate the nice duality between the theorems for
-- folding and unfolding.
--

-- $relation
-- 
-- For this pearl, we need only a one-paragraph introduction to relations.
--
-- 
-- To say that a function @f@:
--
-- @
-- \ f :: a -> b    \"is a subset of\"    (b '><' a)
-- \                     where:         ( y      , x )  ∈   'f'
-- \ means that:
-- \      y  is a result to which
-- \      f  maps 
-- \      x  (the argument) 
-- \ with the constraint: 
-- \                                    ( y      , x )  ∈   'f'
-- \    and                             ( |   y' , x )  ∈   'f'
-- \                                      |   |
-- \    which normally imply:             y = y'
-- @
--
-- This constraint is /relaxed/ for relations, allowing @x@ to be mapped
-- to multiple @ys@.


-- $relationcompose
--
-- Composition of relations is defined by:
--
-- @
-- \  (z, x) ∈ (R . S) == ( (exists y :: (z,y) ∈ R) '/\' ((y,x) ∈ S) )
-- @
--


-- $relationconverse
--
-- Given:
--
-- @ R  :: a -> b @
--
-- Its converse:
--
-- @ R° :: b -> a @
--
-- is defined by:
--
-- @
-- \ (x,y) ∈ R° == (y,x) ∈ R
-- @


-- $relationlaws
-- We therefore have:
--
-- @
-- \       \ \   (  R°  )°   = R        \ \ -- ( ° on (R°) is the original R )
-- \ And:         'id'°      = 'id'         -- ( ° on  'id'  is 'id' )
-- \ Also:     (R '><' S)°   = (R '><' S)   -- ( ° distributes covariantly      over ('><') )
-- \ And:      (R  '.' S)°   = S°  '.' R°   -- ( ° distributes contravariantly  over ('.')  )
-- \                                              
-- @
--
-- In this pearl we view unfolds as the relational converse of folds, which is
-- another reason why we restrict ourselves to finite lists -- /our unfolds/ are
-- actually converses of folds in disguise, and an 'unhom' is the relational converse
-- of a list homomoprhism.
--
-- Let the relation 'mem' relate a list to any of its members:
--
-- @ mem :: [a] -> a @
-- 
-- and let:
--
-- @
--        unfr ('||>') p = 'mem' . 'unfoldrp' ('||>') p
--        unfl ('<||') p = 'mem' . 'unfoldlp' ('<||') p
-- @
--
-- While 'unfoldrp' and 'unfoldlp' generate the entire trace, 'unfr' and 'unfl' map
-- the input seed to one arbitrary intermediate state.
--
-- That is:
--
--
-- @
-- \ 'unfr' ('||>') p
-- \      maps v0 to each of:
-- \                          [ ( []     , v0  ) 
-- \                          , ( [x1]   , v1  ) 
-- \                          , ( [x1,x2], v2  )
-- \                            ...
-- \                          ]
-- \ if:                        ( x_i+1  ,v_i+1) = ('||>') v_i
-- @
--
-- The equality @(5)@ can be expressed relationally:
--
-- If @k = 'unfoldr' ('||>') p@ we have:
--
-- @
-- \ (7)     cat° . 'k' = ('id' '><' 'k' ) . 'unfr' ('||>') p
-- @
--
-- Both sides relate the input seed, say @v@, to a /pair/ of lists.
-- On the left hand side, the list returned by @k v@ is arbitrarily split into @(xs,ys)@.
-- 
-- The equation says that @xs '++' ys = 'k' v@ if and only if @xs@ can be generated by
--    @'unfr' ('||>') p@ and @ys@ can be generated by 'k' starting from where
--    @'unfr' ('||>') p@ left off.
--
-- For @k = 'unfoldl' ('<||') p@, a symmetric property holds:
--
-- @
-- \ (8)     cat° . 'k' = ('k'  '><' 'id') . 'unfl' ('<||') p
-- @
--
-- Recall that our aim is to construct 'g'. It suffices to find a 'g' such that:
--
-- @ cat° . 'k' = ('k' '><' 'k') . 'g' @
--
-- That is:
--
-- @ 
-- \ 'k' v     = xs '++' ys   iff:
-- \ \ \         || \  \ ||   
-- \ \ \         xs \  \ ||   xs and:
-- \ \ \            \  \ ys   ys       can be generated 
-- \ \ \            \   \              from seeds produced by 'g' v.
-- @
--
-- Notice that by applying the converse operator @(_)*@ to both sides of @(7)@ and @(8)@,
-- we get equations that are almost @(3)@ and @(4)@ apart from having converses of 'k' and
-- unfolds in the formulae:
--
-- @
-- \      k° . cat = (unfr ('||>') p)° . ('id' '><' 'k'°)
-- \      k° . cat = (unfl ('<||') p)° . ('k'° '><' 'id')
-- @
--


-- $theorem04
-- The proof of Theorem 2, however, proceeds the same way even if the components
-- are not functions! In the realm of relations, part of the proof of Theorem 2
-- can be generalized to:
--
-- @
-- If:
-- \      R . R° . R     =    R                   (R has converse)
--
-- Then:
--                    R . 'cat' = Sfwd \  \  . ('id' '><' R )
--                        \   \ = Srev \  \  . (R  '><' 'id')
-- { ... }                \   \ = R  . 'cat' . (R° '><' R°) . (R '><' R)
-- @
--
theorem04 :: a
theorem04 = undefined
-- ^

-- $theorem04proof
--
-- /Proof./
--
-- The same as that of Theorem 2. We will prove a more general Theorem 7 later.
theorem04proof :: a
theorem04proof = undefined
-- ^

-- $theorem05
-- The desired dual theorem thus follows:
--
-- @
-- If:
--   'k'  =  'unhom'      'g'  'f'  'p'  'q'
--   \ \     \      \    \ |\ \ |\ \ |\ \ |
--   \ \     \      \    \ ?\ \ ?\ \ ?\ \ ?  -- ^ for some g , f , p , and q
-- Then:\ \  \      \    \  \ \  \ \ |\ \ 
--   'k'  =  'unfoldr' ('||>')\   \ 'p'
--   \ \  =  'unfoldl' ('<||')\   \ 'p'
-- @
--
--

-- |
theorem05 :: a
theorem05 = undefined
-- ^

-- $theorem05proof
-- 
-- /Proof./
--
-- We have already talked about 'f' and 'q', and now we aim to find 'g'
-- such that:
--
-- @
--        'cat'° . 'k' = ( 'k' '><' 'k' ) . 'g'
-- @
--
-- Using properties @(7)@ and @(8)@ as antecedents of Theorem 4 we get:
--
-- @
-- With:
--      (7)    'cat'° . 'k' = ('id' '><' 'k' ) . 'unfr'  ('||>')  'p'
--      (8)    'cat'° . 'k' = ('k'  '><' 'id') . 'unfl'  ('<||')  'p'
-- We get:
--             'cat'° . 'k' = ('k'  '><' 'k' ) . ('k'° '><' 'k'°) . 'cat'° . 'k'
-- @
--
-- Thus @g@ can be any functional subset of:
--
-- @
--  \                                   ('k'° '><' 'k'°) . 'cat'° . 'k'
-- @
--


-- |
theorem05proof :: a
theorem05proof = undefined
-- ^ 




-- $calculating
--
-- The expression:
--
-- @ ('k'° '><' 'k'°) . 'cat'° . 'k' @
--
-- is not easy to simplify. To calculate 'g' we often use Lemma 6 to to refine:
--
-- @ ('k'° '><' 'k'°) . 'cat'° . 'k' @
--
-- @
-- \      (('k'° '><' 'id') '.' 'unfr' ('||>') 'p')
-- \  ∩   (('id' '><' 'k'°) '.' 'unfl' ('<||') 'p')
-- @
--
-- For readers who are not familar with intersection and converses, Figure 1 offers
-- some intuition. 
--
-- Given a seed @v@, and both:
--
-- @
--   'unfr' ('||>') 'p'
--   'unfl' ('<||') 'p'
-- @
--
-- which non-deterministically pause somewhere and respectively generate @(xs,v2)@ and @(v1,ys)@.
-- The intersection means that they stop when they \"meet in the middle\", that is, when
-- 
-- @
--   'k' v1 = xs 
--   'k' v2 = ys
-- @
-- 
-- The new pair of seeds, a possible result of 'g', is @(v1,v2)@.


-- $figure01
--
-- @
-- \                  ------\\
-- \                  || ||  \\ 
-- \                  || ||   \\
-- \                 (xs,v2)   \\
-- \                 /^         \\ 'unfr' ('||>') 'p'
-- \              'k' /            \\ 
-- \               /              \\
-- \        (v1,v2)               (v)
-- \               \\              /
-- \              'k' \\            /
-- \                 \\v         / 'unfl' ('<||') 'p' 
-- \                 (v1,ys)\   /
-- \                  || || \  /
-- \                  || || \ /
-- \                  ------/
-- \                       
-- @


-- $theorem06
--
-- This lemma has a functional formulation that may be more friendly to readers. Writing
-- the list membership predicate @'elem' :: a -> [a] -> Bool@ in the Haskell Standard Prelude
-- in infix position as @(∈)@, we have:
--
-- Assume that:
--
-- @
--   'k'  =  'unfoldr' ('||>')\   \ 'p'
--   \ \  =  'unfoldl' ('<||')\   \ 'p'
-- @
-- 
-- A function 'g' is a subset of:
--
-- @
--  \                                   ('k'° '><' 'k'°) . 'cat'° . k
-- @
--
-- If, /for all v/:
--
-- @
--       'g' v = (\ \ v1,\ \  v2)
-- Where:
--       \ \     ('k' v1,\ \  v2) ∈ 'unfoldrp' ('||>') 'p' v
--       \ \     (\ \ v1, 'k' v2) ∈ 'unfoldlp' ('<||') 'p' v
-- @
--
-- From now on we will use Lemma 6 and restrict ourselves to functions when we calculate 'g'.

-- |
theorem06 :: a
theorem06 = undefined
-- ^ 


(/\) :: (a -> b) -> (a -> c) -> a -> (b,c)
(f /\ g) x = (f x, g x)




h_sum [] = 0
h_sum [x] = (+) x
--h_sum (l ++ r) = f (h l, h r)


-- curry :: ((a,b) -> c) -> a -> b -> c

k = undefined
g = undefined
f = undefined
p = undefined
q = undefined
unfl = undefined
unfr = undefined
(|<|) = undefined
(|>|) = undefined



uncurry1 :: (a -> b)           -> (a)     -> b
uncurry2 :: (a -> b -> c)      -> (a,b)   -> c
uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d

uncurry1 f (x) = f x
uncurry2 f (x,y) = f x y
uncurry3 f (x,y,z) = f x y z


