{-# LANGUAGE ViewPatterns, PatternGuards #-}
module Pearl.Generalizing_and_Dualizing_the_Third_List_Homomorphism_Theorem.Section02 where

import Pearl.Generalizing_and_Dualizing_the_Third_List_Homomorphism_Theorem.Ref


-- * The Third-List Homomorphism Theorem

h :: [a] -> b
h = undefined
-- ^ As is well known, in the world of sets and total functions, the equations:
-- 
-- @
--  (1)       'h'  [    ]  =   'e'
--  (1)       'h'  (x:xs)  = ('<||') (x, 'h' xs)
-- @
--
-- have a unique solution for @'h' :: [a] -> b@, denoted by:
--
-- @
--            'h'          = 'foldr' ('<||') 'e'
-- @
--

e :: b
e = undefined
-- ^
-- e is the unit for the fold, ie, it is like the @[]@ we replace at the end of a list.


(<||) :: (a,b) -> b
(<||) = undefined
-- ^
-- We deviate from the standard and let ('<||')  be uncurried since it is more convenient in point-free style,
-- where programs are described by function composition rather than application. 


-- |
-- In fact, we will also introduce uncurried constructor: 
-- @
--   cons ( x, xs ) = x ∶ xs
-- @
--
cons :: (a, [a]) -> [a]
cons (x,xs)     = x:xs

(><) :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
(f >< g) (x,y) = (f x, g y)
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


-- |
-- Thus (1) can be written
-- 
-- @ (1a)  'h' '.' 'cons' = ('<||') '.' ('id' '><' 'h') @
--

a1 = h . cons


-- ** Deriving 'foldrr' from 'foldr'
foldrr :: ((a,b) -> b) -> ([a], b  ) -> b
foldrr (<||) ( []  ,  e ) = e
foldrr (<||) ( x:xs,  e ) = (<||) (  x  , foldrr (<||) (xs,e)  )

-- ^
-- We define a variation of 'foldr' that takes the base case as an extra argument.
--
-- It can be seen as a @resumed@ version of 'foldr' (hence the suffix @r@ in the name).
--
-- That is, if:
--
-- @    'h'              = 'foldr' ('<||') 'e' @
--
-- then one can show:
-- 
-- @(2) 'h' (xs '++' ys)   = 'foldrr' ('<||') ( xs , 'h' ys )@
--
-- by induction on xs.
--

cat :: ([a],[a]) -> [a]
cat (xs,ys) = xs ++ ys
-- ^
-- Let:
--
-- @ 'cat' (xs,ys) = xs '++' ys @
-- 
-- then equation (2):
--
-- @(2) 'h' (xs '++' ys) = 'foldrr' ('<||') ( xs , 'h' ys )@
-- 
-- can be point-free as:
--
-- @(3) 'h' '.' 'cat'      = 'foldrr' ('<||') '.' ( 'id' '><' 'h' ) @
--


-- ** Deriving 'foldlr' from 'foldl'
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
-- It is known that 
--
-- @ 'foldl' ('||>') 'e' @
--
-- is the unique solution for @'h' :: [a] -> b@
--
-- in:
--
-- @
--        'h' [] = 'e'
--        'h' '.' 'snoc' = ('||>') '.' ( 'h' '><' 'id' )
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

-- ^
-- we have, if @ 'h' = 'foldl' ('||>') 'e' @, that:
--
-- @ (4)  'h' '.' 'cat' = 'foldlr' ('||>') '.' ( 'h' '><' 'id' ) @
--


-- ** A List Homomorphism

-- |
-- A function @ 'hom' :: [a] -> b @ is a list homomoprhism if there exists:
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
--   hom [] = e
--   hom [x] = k x
--   hom (xs '++' ys) = f ('hom' xs, 'hom' ys)
-- @
--
-- In such a case, we denote 'h' by @'hom' f k 'e'@.
--
-- The equations imply that @f@ is associative, on the range of 'h', with unit 'e'. 
-- To compute a list homomorphism 'h', one may
--
-- 1. split the list arbitrarily into two parts
--
-- 2. recursively compute 'h' on both parts
--
-- 3. and combine the results using @f@
--
-- implying a potential for parallel computation. If @f@ and @k@ are constant-time
-- operations, a list homomorphism can be evaluted in time @ O( n / p + log p )@
--
-- where
--
-- * @n@ is the length of the list, and
--
-- * @p@ is the number of processors
--
-- 
-- resulting in almost linear speedups with respect to @p@.

hom :: [a] -> b
hom = undefined


-- * List Homomorphism Theorems

-- ** Second List Homomorphism Theorem
-- |
-- (the 2nd list-homomorphism theorem [@'ref01'@]).
-- #theorem01#
--
-- If
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
theorem01 :: a
theorem01 = undefined


-- ** Third List Homomorphism Theorem 
-- #theorem02#

theorem02 :: a
theorem02 = undefined
-- ^
-- Somewhat surprisingly, if a function can be computed both by a 'foldr' and a 'foldl',
-- it /is/ a list homomorphism:
-- 
-- (the 3rd list-homomorphism theorem [@'ref06'@])
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

theorem02proof :: a
theorem02proof = undefined
-- ^
-- /Proof./
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
-- While a semantical proof was given by [@Gibbons 'ref06'@], we will provide a proof
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
-- Thus the theorem holds if we pick
--
-- @
--                    f  = 'h' . 'cat'   . ('hI' '><' 'hI')
-- @
--

-- |
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
-- that 'h' is both a 'foldr' and a 'foldl', could be weakened - properties (3) and (4) 
-- were merely used to push 'h' to the right.  In fact, @'h' . 'cat'@ is never expanded in the proof.
-- 
-- One thus wonders whether there is something more general waiting
-- to be discovered, which is indeed what we will see in the following sections. 
-- The syntactical approach makes such generalisations much easier to spot.
-- 
theorem02comments :: a
theorem02comments = undefined
