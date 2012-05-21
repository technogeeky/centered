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


-- ** 'foldlr' from 'foldl'
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
hom :: [a] -> b
hom = undefined






unhom g f p q = k
     where 
     k v | p v = []
         | q v = [f v]
         | (l,r) <- g v = k l ++ k r






unfoldr :: (b -> (a,b)) -> (b -> Bool) -> b -> [a]
unfoldl :: (b -> (b,a)) -> (b -> Bool) -> b -> [a]

unfoldrp :: (b -> (a,b)) -> (b -> Bool) -> b -> [([a], b)]
unfoldlp :: (b -> (b,a)) -> (b -> Bool) -> b -> [(b, [a])]


unfoldr (||>)    p v 
               | p v                    = []
               | (x, v') <- (||>) v     = x : unfoldr (||>) p v'

unfoldl (<||)    p v
               | p v                    = []
               | (v', x) <- (<||) v     = unfoldl (<||) p v' ++ [x]
-- |
--   (2)       h (l ++ r) = foldrr (<||) (l, h r) if     h  =  foldr  (<||)  e
--
-- |
--   (3)       h . cat = foldrr  (<||) . (id, h)

unfoldrp (||>) p v = (|>|) ([], v) where (|>|) (xs, v) | p v                = [(xs, v)]
                                                       | (x, v') <- (||>) v = (xs, v) : (|>|) (xs ++ [x], v')
unfoldlp (<||) p v = (|<|) (v, []) where (|<|) (v, xs) | p v                = [(v,xs)]
                                                       | (v', x) <- (<||) v = (|<|) (v', x:xs) ++ [(v,xs)]



-- * from AGDA:

(/\) :: (a -> b) -> (a -> c) -> a -> (b,c)
(f /\ g) x = (f x, g x)




h_sum [] = 0
h_sum [x] = (+) x
--h_sum (l ++ r) = f (h l, h r)


-- curry :: ((a,b) -> c) -> a -> b -> c



uncurry1 :: (a -> b)           -> (a)     -> b
uncurry2 :: (a -> b -> c)      -> (a,b)   -> c
uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d

uncurry1 f (x) = f x
uncurry2 f (x,y) = f x y
uncurry3 f (x,y,z) = f x y z

