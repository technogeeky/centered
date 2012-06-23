{-# LANGUAGE ViewPatterns, PatternGuards, ImplicitParams #-}
------------------------------------------------------------------------------------------------
-- |
-- Module      :  Pearl.GaDtTLHT.Section02
-- Description :  The Third-List Homomorphism Theorem
-- Copyright   :  (c) Drew Day 2012
--                (c) Shin-Cheng Mu 2011
--                (c) Akimasa Morihata 2011
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Drew Day <drewday@gmail.com>
-- Stability   :  stable
-- Portability :  portable (ViewPatterns, PatternGuards)
--
------------------------------------------------------------------------------------------------
module Pearl.GaDtTLHT.Section02 
     ( 
     -- ** The Third-List Homomorphism Theorem
     h
     , p01
     , p01a
     , e
     -- ** Uncurrying some familiar functions
     , (<||)
     , (||>)
     , cons
     -- ** a product function
     , (><)
     , p01b
     -- ** foldrr: a resumed foldr
     , foldrr
     , p02
     , cat
     , p03
     -- ** DLists
     , unsnoc
     , snoc
     , p03b
     , p03h
     -- ** foldlr: a resumed foldl
     , foldlr
     , p04
     -- ** A List Homomorphism
--     , h
     -- * Some Theorems

     -- ** Theorem 1
     , t01
     -- *** Proof
     , t01proof

     -- ** Theorem 2
     , t02
     -- *** Proof
     , t02proof
     -- *** Comments
     , t02comments
     , p04pick
     ) where

import Pearl.GaDtTLHT.References
import Data.List (sortBy)


-- $p01
-- p01 :: [a] -> b
p01 []     = e
p01 (x:xs) = (<||) (x, p01 xs)

-- ^
-- As is well known, in the world of sets and total functions, the equations:
-- 
-- @
--   ('p01')       'h'  [    ]  =   'e'
--   ('p01')       'h'  (x:xs)  = \    \  ('<||') (x, 'h' xs)
--   ('p01a')      'h'          = 'foldr' ('<||') 'e'
-- @
-- 
-- have @('p01a')@ as unique solution for @'h' :: [a] -> b@.

p01a :: property (foldr) with (a leftwards function) and (a unit e) is unique
p01a = undefined
-- ^
-- @
--   ('p01a')      'h'          = 'foldr' ('<||') 'e'
-- @


e :: property (e is a unit for a fold)
e = undefined
-- ^
-- e is the unit for the fold, ie, it is like the @[]@ we replace at the end of a list.


(<||) :: (a,b) -> b
(<||) = snd

(||>) :: (a,b) -> a
(||>) = fst



-- ^
-- We deviate from the standard and let ('<||')  be uncurried since it is more convenient in point-free style,
-- where programs are described by function composition rather than application. 


-- |
-- In fact, we will also introduce uncurried constructor:
--
-- @
--   cons ( x, xs ) = x ∶ xs
-- @
--
cons :: (a, [a]) -> [a]
cons (x,xs)     = x:xs

uncons :: [a] -> (a, [a])
uncons (l:r:[]) = (l,[r])
uncons ________ = error "i didn't write this correctly"

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

-- ^ 
-- and let: 
-- 
-- @ ( f '><' g ) ( x, y ) = ( f x, g y ) @
-- 
-- which satisfies a law:
-- 
-- @ ( f '><' g ) '.' ( h '><' k ) = ( f '.' h '><' g '.' k ) @
-- 
-- which we will refer to as @product functor@. 


p01b :: property (p01) [pointfree style]
p01b = undefined
-- ^
-- Thus ('p01'):
-- 
-- @
--   ('p01')       'h'  [    ]  =   'e'
--   ('p01')       'h'  (x:xs)  = ('<||') (x, 'h' xs)
-- @
-- 
-- can be written:
-- 
-- @
--   ('p01b')     'h' '.' 'cons' = ('<||') '.' ('id' '><' 'h')
-- @
--



foldrr :: ((a,b) -> b) -> ([a], b) -> b
foldrr (<||) ( []  ,  e ) = e
foldrr (<||) ( x:xs,  e ) = (<||) (  x  , foldrr (<||) (xs,e)  )

-- ^
-- We define a variation of 'foldr' that takes the base case as an extra argument.
--
-- It can be seen as a @resumed@ version of 'foldr' (hence the suffix @r@ in the name).
--

p02 :: property (foldrr is foldr [resumed])
p02 = undefined
-- ^
-- That is, if:
--
-- @
--   ('p01a')      'h'          = 'foldr' ('<||') 'e'
-- @
--
-- then one can show:
-- 
-- @
--   ('p02') 'h' (xs '++' ys)   = 'foldrr' ('<||') ( xs , 'h' ys )
-- @
--
-- by induction on xs.
--

cat :: ([a],[a]) -> [a]
cat (xs,ys) = xs ++ ys

p03 :: property (p02) [pointfree style]
p03 = undefined
-- ^
-- Let:
--
-- @ 'cat' (xs,ys) = xs '++' ys @
-- 
-- then equation ('p02'):
--
-- @('p02') 'h' (xs '++' ys) = 'foldrr' ('<||') ( xs , 'h' ys )@
-- 
-- can be point-free as:
-- 
-- @('p03') 'h' '.' 'cat'      = 'foldrr' ('<||') '.' ( 'id' '><' 'h' ) @
--


unsnoc :: [a] -> ([a],a)
unsnoc (l:r:[]) = ([l],r)
unsnoc ________ = error "i didn't write this correctly"

snoc :: ([a],a) -> [a]
snoc (xs,x) = xs ++ [x]

-- ^
--
-- Symmetrically, let:
--
-- @ 'snoc' (xs, x) = xs '++' [x] @
--

p03b :: property (foldl with (||>) (pronounced forward)) is the unique solution for snoclists
p03b = undefined
-- ^
-- It is known that 
--
-- @ 'foldl' ('||>') 'e' @
--
-- is the unique solution for @'h' :: [a] -> b@
--
-- in:
--

p03h :: property (p03b) [pointfree]
p03h = undefined
-- ^
-- @
--   ('p03h')  'h' [] = 'e'
--             'h' '.' 'snoc' = ('||>') '.' ( 'h' '><' 'id' )
-- @
--
-- where @ ('||>') :: (b,a) -> b. @


-- |
-- Defining @resumable@ 'foldl' as:
--
-- @
--   'foldlr' ('||>') ( 'e'   ,  []              ) = 'e'
--   'foldlr' ('||>') ( 'e'   , 'unsnoc' -> (xs,x) ) = ('||>') ( 'foldlr' ('||>') ('e',xs)  ,  x  ) 
-- @
--
foldlr :: ((b,a) -> b) -> ( b , [a]) -> b
foldlr (||>) ( e   ,  []              ) = e
foldlr (||>) ( e   , unsnoc -> (xs,x) ) = (||>) ( foldlr (||>) (e,xs)  ,  x  )


p04 :: property (dual to p03) [pointfree]
p04 = undefined
-- ^
-- We have, if @ 'h' = 'foldl' ('||>') 'e' @, that:
-- 
-- @ ('p04')  'h' '.' 'cat' = 'foldlr' ('||>') '.' ( 'h' '><' 'id' ) @


h :: property (h is a list homomorphism) denoted (hom f k e)
h = undefined
-- ^
-- A function @ 'h' :: [a] -> b @ is a [/list homoprhism/] if there exists:
--
-- @ 'e' :: b @
--
-- @ 'k' :: a -> b @
--
-- @ 'f' :: (b '><' b) -> b @
--
-- such that:
-- 
-- @
--   h [] = e
--   h [x] = k x
--   h (xs '++' ys) = f ('h' xs, 'h' ys)
-- @
--
-- In such a case, we denote 'h' by @'hom' f k 'e'@.
--
-- The equations imply that @f@ is associative, on the range of 'h', with unit 'e'. 
--
-- To compute a list homorphism 'h', one may
--
-- 1. split the list arbitrarily into two parts
--
-- 2. recursively compute 'h' on both parts
--
-- 3. and combine the results using @f@
--
-- implying a potential for parallel computation. If @f@ and @k@ are constant-time
-- operations, a list homorphism can be evaluted in time @ O( n / p + log p )@
--
-- where
--
-- * @n@ is the length of the list, and
--
-- * @p@ is the number of processors
--
-- 
-- resulting in almost linear speedups with respect to @p@.


t01 :: theorem (second list homomoprhism theorem)
t01 = undefined
-- ^
-- [/Theorem/:]
-- (the 2nd list-homomorphism theorem [@'r01'@]).
--
-- If
--
-- @
--        'h' = 'hom' f k 'e'
-- @
--
-- Then
--
-- @
--        'h' = 'foldr' '<||' 'e'
--        'h' = 'foldl' '||>' 'e'
-- @
--
-- Where
--
-- @
--        ('<||') (x, v) = f (k x,   v)
--        ('||>') (v, x) = f (  v, k x)
-- @
--


t01proof :: theorem proof [t01]
t01proof = undefined
-- ^
-- [/Proof./]
--
-- A complete proof is given in [@'r01'@].
--


t02 :: theorem (third list homomorphism theorem)
t02 = undefined
-- ^
-- 
-- Somewhat surprisingly, if a function can be computed both by a 'foldr' and a 'foldl',
-- it /is/ a list homomorphism.
-- 
-- [/Theorem/:]
-- 
-- (the 3rd list-homomorphism theorem [@'r06'@])
-- 
-- @
--        'h' = 'foldr' '<||' 'e'
--        'h' = 'foldl' '||>' 'e'
-- @
-- 
-- implies
--
-- @
--        'h' = 'hom' f k 'e'
-- @
--
-- for some @f@ and @k@.

t02proof :: theorem proof [t02]
t02proof = undefined
-- ^
-- [/Proof./]
--
-- The only possible choice for @k@ is:
--
-- @
--        'k' x = 'h' [x]
-- @
--
-- The aim is to find @f@ such that:
--
-- @
--        'h' . 'cat' = f . ('h' '><' 'h')
-- @
--
-- A function @fI@ is called a /right inverse/ of @f@ if, for all @y@ in the
-- range of @f@, we have @f (fI y) = y@. Equivalently, @f . fI . f = f@. 
-- 
-- In a set-theoretical model, a right inverse always exists but may not be unique.
--
-- While a semantical proof was given by [@Gibbons 'r06'@], we will provide a proof
-- having a much more equational flavour.
--
-- We reason:
--
-- @
--                        'h' . 'cat'
-- { use (3) }           =
--                        'foldrr' '<||' . ('id' '><' 'h')
-- { h = h . hI . h
--  and product functor} =
--                        'foldrr' '<||' . ('id' '><' 'h')   . ('id' '><' 'hI') . ('id' '><' 'h')
-- { (3) backwards
--  and (4) forwards }   =
--                        'foldlr' '||>' . ('h'  '><' 'id')  . ('id' '><' 'hI') . ('id' '><' 'h')
-- { h = h . hI . h
--  and product functor} =
--                        'foldlr' '||>' . ('h'  '><' 'id')  . ('hI' '><' 'hI') . ('h' '><' 'h')
-- { (4) backwards }     =
--                         'h' . 'cat'   . ('hI' '><' 'hI')  . ( 'h' '><' 'h' )
-- @
--

p04pick :: pick
p04pick = undefined
-- ^
-- Thus the theorem holds if we pick
--
-- @
--                    f  = 'h' . 'cat'   . ('hI' '><' 'hI')
-- @
--

t02comments :: theorem comments [t02]
t02comments = undefined
-- ^
-- /Comments./
--
-- Theorem 2 in fact provides hints how to construct list homomorphisms. 
-- 
-- For example, since @sum = foldr (+) 0 = foldl (+) 0@, Theorem 2 states that sum can be written as:
-- 
-- @
--      'sum' = 'hom' f k 0 
--        where k x        = sum [ x ] = x 
--              f ( v, w ) = sum ( g v ++ g w ) 
-- @
-- 
-- for any right inverse @g@ of 'sum'. 
-- 
-- One may simply pick 
-- 
-- @
--      g x = [x]
-- @
-- 
-- and @f (v,w)@ simplifies to @v + w@.
-- 
-- Readers might have noticed something odd in the proof: the property much talked about, 
-- that 'h' is both a 'foldr' and a 'foldl', could be weakened - properties ('p03') and ('p04') 
-- were merely used to push 'h' to the right.  In fact, @'h' . 'cat'@ is never expanded in the proof.
-- 
-- One thus wonders whether there is something more general waiting
-- to be discovered, which is indeed what we will see in the following sections. 
-- The syntactical approach makes such generalisations much easier to spot.
-- 

