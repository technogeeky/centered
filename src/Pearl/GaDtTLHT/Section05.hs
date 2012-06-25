------------------------------------------------------------------------------------------------
-- |
-- Module      :  Pearl.GaDtTLHT.Section05
-- Description :  Generalising to Trees
-- Copyright   :  (c) Drew Day 2012
--                (c) Shin-Cheng Mu 2011
--                (c) Akimasa Morihata 2011
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Drew Day <drewday@gmail.com>
-- Stability   :  stable
-- Portability :  portable (ViewPatterns, PatternGuards)
--
------------------------------------------------------------------------------------------------
module Pearl.GaDtTLHT.Section05 where

import Pearl.GaDtTLHT.Section03 (t05)

t07 :: theorem [on relations]
t07 = undefined
-- ^
-- [/Theorem/.]
--
-- As mentioned earlier, when looking at the proof of Theorem 2 in detail, one notices that further generalization
-- is possible. Indeed, the presence of @R . cat@ in Theorem 4 is superficial, and the theorem can still be
-- generalized:
-- 
-- @
-- R   = U . (id '><' S )    -- (RUS)
--     = V . (T  '><' id)    -- (RVT)
-- @
-- 
-- where
-- 
-- @
--     S . S° . S = S        -- (S invertable)
--     T . T° . T = T        -- (T invertable)
-- @
-- 
-- imply
-- 
-- @
-- R   = R . (T° '><' S°) . (T '><' S)
-- @


t07proof :: theorem proof [t07] [on relations]
t07proof = undefined
-- ^
-- [/Proof/:]
--
-- @
--                     R = U . (id x S)
--  { S invertable }     = U . (id x S) . (id x S°) . (id x S)
--  { RUS, in reverse }  = R            . (id x S°) . (id x S)
--  { RVT }              = V . (T x id) . (id x S°) . (id x S)
--  { T° T = id ? }      = V . (T x id) . (T° x S°) . (T x S)
--  { RVT, in reverse }  = R            . (T° x S°) . (T x S)
--
--  { done }           R = R . (T° x S°) . (T x S)
-- @
--


t07comments :: theorem comments [t07]
t07comments = undefined
-- ^
-- [/Comments/:]
--
-- However, the conclusion of the theorem, that:
--
-- @R = R . (T° x S°) . (T x S)@
--
-- does not have much structure hinting at how this theorem can be useful. 
--
-- The use cases we have found are when @S@ is a sub-expression of @R@ 
-- (for example, to prove [@'t05'@], we used:
--
-- @ 
-- \ R     = k° . cat
-- \ S = T = k°
-- @
--
-- We do not require @S = T@ in general. 
--
-- We use Theorem 7 to establish recursive equations about @S@, hoping to 
-- construct a terminating definition of @S@.



-- 5.1   Third Homomorphism Theorem on Trees Revisited
-- 
-- Consider the Tree datatype defined in Section 4.2 and assume that
-- we wish to efficiently compute a function on trees by distributing
-- the work to multiple processors. One slight annoyance is that split
-- ing a tree into two at an arbitrary point yields not two trees, but
-- one tree and a context containing a single hole that can be filled
-- by a tree. The concept of contexts of a datatype was proposed by
-- Huet [@'r08'@] as the zipper. In particular, the context for Tree can be
-- modelled by
-- 
-- @
-- type Cxt a = [ Z a ]
-- data Z a = Nl a ( Tree a )  
--          | Nr   ( Tree a ) a
-- @
-- 
-- with a function fill :: ( Cxt a, Tree a ) -> Tree a that fills the hole
-- of the context by a tree:
-- 
-- @
-- fill ([ ] , t ) = t
-- fill ( Nl x u : xs, t )  = fill ( xs, N t x u )
-- fill ( Nr t x : xs, u ) = fill ( xs, N t x u ) .
-- @
-- 
-- For example, consider the tree
-- 
-- @
-- t = N ( N ( L 1 ) 2 ( L 3 )) 4 ( N ( N ( L 5 ) 6 ( L 7 )) 8 ( L 9 ))
-- @
-- 
-- What remains after taking out the subtree u = N ( L 5 ) 6 ( L 7 ) is
-- 
-- @
-- cx = [ Nl 8 ( L 9 ) , Nr ( N ( L 1 ) 2 ( L 3 )) 4 ]
-- @
-- 
-- with fill ( cx, u ) = t.
-- 
-- 
-- To parallelise a function f :: Tree  a -> b, we must have a
-- variation f' :: Cxt  a -> b defined on contexts. 
--
-- Instantiating R, S, and T in Theorem 7 respectively to:
--
-- @
--   R = f . fill
--   S = f
--   T = f'
-- @
-- 
-- we see that f t can be computed in terms of f' cx and f u,
-- 
-- @
-- f . fill = f . fill . ( f' . '><' f . ) . ( f' '><' f ) ,
-- @
-- 
-- if there exist U and V such that
--
-- @
--   ('p11')   f . fill = U . ( id '><' f ) ∧
--   ('p12')   f . fill = V . ( f′ '><' id )
-- @

p11 :: property
p11 = undefined
-- ^
-- @
--   ('p11')   f . fill = U . ( id '><' f ) ∧
-- @

p12 :: property
p12 = undefined
-- ^
-- @
--   ('p12')   f . fill = V . ( f′ '><' id )
-- @


-- 
-- 
-- Equation ('p11') basically says that f is outwards:  
--
-- @f ( fill ( cx, u ))@ can be computed from @cx@ and @f u@
--
-- while ('p12') says that f is also inwards:
--
-- @f ( fill ( cx, u ))@ can be computed from @f′ cx@ and @u@.
-- 
-- This is another way to view the previous work of Morihata et
-- al. [@'r10'@].
-- 

--
-- 5.2   Generating Trees from the Middle
-- 
-- One naturally wonders whether the result can also be dualised to
-- generating trees rather than consuming them. Again the answer is
-- yes: if a tree can be generated both upwards and downwards, it can
-- be generated /from the middle/. We will formalise what we mean
-- below.
-- 
-- 
-- Let b be the type of seeds. Unfolding a tree downwards from the
-- root using a function g↓ :: b -> ( b, a, b ) is relatively familiar:


test55 = undefined


-- @
-- (|↓|) ::  ( b -> ( b, a, b ) , b -> a, b -> Bool ) -> b -> Tree a 
-- (|↓|) (fs@ ( g↓, f↓, p )) v    | p v
--                               = L ( f↓ v )
--                               | (v1,x,v2) <- g↓ v      
--                               = N ( unf↓ fs v1 ) x ( unf↓ fs v2 )
-- @
-- 
-- As in the case of lists, our unfolds are actually converses of folds
-- in disguise and are well-defined only if they terminate and produce
-- finite trees. We consider paused versions of unfolding, which yields
-- ( Cxt a, b ) , a context and a seed. The function unfp↓ returns a list
-- containing all such pairs of contexts and seeds:
-- 
-- @
-- unfp↓ ::  ( b -> ( b, a, b ) , b -> a, b -> Bool ) -> b -> [( Cxt a, b )]
-- unfp↓ ( fs@ ( g↓, f↓, p )) v = iter↓ ([],v) 
--      where
--           iter↓ ( xs, v ) | p v = [( xs, v )]
--                           | ( v1 , x, v2 ) <- g↓ v 
--                           = ( xs, v ) : iter↓ ( Nl x ( unf↓ fs v2 ) : xs, v1 ) 
--                                     ++ iter↓ ( Nr ( unf↓ fs v1 ) x : xs, v2 )
-- @
-- 
-- 
-- With the definition we have, for k = unf↓ ( g↓, f↓, p ) , that
-- 

p13 :: property
p13 = undefined
-- ^
-- @
--   ('p13')   fill° . k = ( id '><' k ) . mem . unfp↓ ( g↓, f↓, p )
-- @
-- 
-- By generating a tree “from the middle” we mean to find:
--
-- @
-- \    g  :: b -> ( b, b )
-- \    k′ :: b ->  Cxt a 
-- @
--
-- such that:
-- 
-- @
-- k v  | p v = L ( f↓ v )
--      | ( v↑, v↓ ) <- g v = fill ( k' v↑ ) ( k v↓ )
-- @
-- 
-- 
-- That is, the tree returned by k, if not a leaf, can be split into a
-- context and a subtree that can be generated separately from the two
-- seeds returned by g. 
--
-- It suffices for g and k'to satisfy:
-- 
-- @
-- \      fill° . k = (k '><' k ) . g
-- @
-- 
-- 
-- Generating a tree “upwards” intuitively means to start from a
-- leaf and find the path back to the root. 
--
-- With application of Theorem 7 in mind, we want to come up with a function:
-- 
-- @
--   unfp↑ ( g↑, f↑ ) :: b -> [( b, Tree a )]
-- @
-- 
-- that satisfies, for some k':
-- 

p14 :: property
p14 = undefined
-- ^
-- @
--   ('p14')   fill° . k = ( k' '><' id ) . mem . unfp↑ ( g↑, f↑ )
-- @


-- Thus each of ('p13') and ('p14') matches one antecedent of Theorem 7.


-- To grow a tree upwards from a leaf, we use a function:
--
-- g↑ :: G↑ a b = b -> [( b, ( a, b ) + ( b, a ))] 
--
-- (where data a + b = Lt a | Rt b).
-- 
-- If @g↑  v@ is empty, we have reached the root.
-- 
-- If it contains @( v', Lt ( x, v r ))@, we go up by using:
--
-- * the current tree as the left child
--
-- * @v r@ the seed for the right child
--
-- * and @v'@ the seed going up. 
-- 
-- Similarly with Rt. 
-- 
-- We could have grown a @Tree a@. To reuse the function later, however, we grow a context instead. 
-- 
-- Define
-- 
-- cxtp↑ g↑ :: b -> [( b, Cxt a )] 
--
-- that generates all (seed, context) pairs when one goes upwards:
-- 
-- @
-- cxtp↑ g↑ v = iter↑ ( v, [ ])
--      where
--              iter↑ ( v, cx ) = ( v, cx ) 
--                          : [ y | ( v', lr ) <- g↑ v, y <- iter↑ ( v', cx ++ [ up lr ])]
-- 
-- up ( Lt ( x, v r )) = Nl x ( k v r )
-- up ( Rt ( v l , x )) = Nr ( k v l ) x.
-- @


-- 
-- To generate all the splits, we need to be able to start from any leaf.
--
-- Thus we let:
--
-- @
--   f↑ :: b -> [( b, a )] 
-- @
--
-- return the list of values on each leaf, paired with a seed to go up with. 
-- 
-- We may then define unfp↑ by:
-- 
-- @
-- unfp↑ ::  ( G↑ a b, b -> [( b, a )]) -> b -> [( b, Tree a )]
-- 
-- unfp↑ ( g↑, f↑ ) v = [( v'' , fill ( cx, L x )) | ( v', x ) <- f↑ v, ( v'', cx ) <- cxtp↑ g↑ v' ]
-- @
-- 
-- While cxtp↑ and unfp↑ return the entire history of upwards tree generation, 
-- their non-pausing version, cxt↑  and unf↑, keep only
-- completed contexts and trees (those which have reached the root):
-- 
-- @
-- cxt↑ g↑ v = [ cx | ( v', cx ) <- cxtp↑ g↑ v, null ( p v' )]
-- 
-- unf↑ ( g↑, f↑ ) v = [ t | ( v', t ) <- unfp↑ ( g↑, f↑ ) v, null ( p v' )]
-- @
-- 
-- 
-- Equation (14) is satisfiable if 
-- 
-- @
--        k = mem . unf↑ ( g↑, f↑ ) 
-- @
-- 
-- is a function (that is, all routes from leaves to the root yields the same tree), and
-- 
-- @
--        k' = mem . cxt↑ g↑ v.
-- @
-- 
-- 
-- With ('p13') and ('p14'), we therefore obtain from Theorem 7 that
-- 

p15 :: property
p15 = undefined
-- ^
-- @
-- \ ('p15')   fill° . k   = ( k' '><' k ) . ( k'° '><' k° ) . fill° .k
--    \   \  if        k   = unf↓ ( g↓, f↓, p ) = mem .  unf↑ ( g↑, f↑ )
--    \   \            k'  = mem . cxt↑ g↑ v. 
-- @
-- 
-- To compute k “from the middle”, we may pick g to be a subset of 
-- 

p15pick :: pick
p15pick = undefined
-- ^
-- @
-- \ ('p15pick')    ( k'° '><' k°) . fill° . k
-- @
