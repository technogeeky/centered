{-# LANGUAGE ViewPatterns, PatternGuards #-}
-- |
-- As multicore hardware has become standard in recent years, parallel programming rekindles as an important potential
-- application of functional programming. The skeletal parallel programming [@'ref03'@] paradigm proposes the idea of
-- developing parallel programs by combining parallel skeletons - functions that capture useful parallel programming
-- patterns. 
--
-- Among the important parallel skeletons is list homomorphism [@'ref01'@], one that satisﬁes the equation:
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
--
-- A well-known third list-homomorphism theorem [@'ref03'@] says that a function is a list homomorphism if it can be described
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
-- 
-- One naturally wonders whether (⊙) can be mechanically constructed.
--
-- Such methods have been proposed (
--        [@'ref05'@], 
--        [@'ref09'@], 
--  and   [@'ref11'@] )
--  and even generalised to trees: [@'ref10'@].
--
-- 
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
-- 
-- Is it true that any function that can be defined as both an 'Data.Sequence.unfoldr' and an 'Data.Sequence.unfoldl' can be written as one that generates the list from the middle?
-- 
-- We show in this pearl that the answer is @positive@. This is not only of theoretical interest but could also have a practical impact. First, there are several efficient algorithms that are based on divide-and-conquer sequence generation, such as Quicksort. Moreover, the performance bottleneck in distributed parallel computing often lies in data distribution. Being able to generate the list anywhere allows us to distribute seeds of sublists and simultaneously generate from them, and thereby reduce communication costs and increase parallelism.
--
-- List homomorphisms and the third list-homomorphism theorem are reviewed in Section 2, before we present a dualised
-- theorem in Section 3 and apply it, in Section 4, to examples including sorting and parallel scan. In Section 5, the
-- results are further generalised to trees, before we conclude in Section 6.
-- 
-- NOTE:
--
-- The rest of the documentation in this section (including 'ccons', 'cons', 'cconcat', 'cat', ...) are provided
-- to demonstrate and witness the conversion from traditional point-full Haskell syntax to point-free Haskell syntax. This
-- should not be strictly necessary, but some programs are easier to express in one form or another. These examples were
-- motivated by, but not included in, the original paper ('ref00').
--
module Pearl.Generalizing_and_Dualizing_the_Third_List_Homomorphism_Theorem.Section01 where
import Pearl.Generalizing_and_Dualizing_the_Third_List_Homomorphism_Theorem.Ref


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



-- * Uncurrying and Point-Free Syntax
--
-- Normal (or point-full) Haskell uses 'curry'-ed functions and function application ('ap', but really just
-- space) to programs.
--
-- In constrast to this, point-free Haskell syntax often uses 'uncurry'-ed functions and function composition ('.')
-- to describe programs.
--
-- Some examples of both styles, and examples on converting between them, follow.



-- ** List Constructor
ccons = (:)
-- ^
-- First, we uncurry the normal (curried) Haskell list constructor (':'), (which I've simply named @ccons@):

-- ^
-- >>> 1 : [2,3]
-- [1,2,3]

-- ^
-- >>> 1 `ccons` [2,3]
-- [1,2,3]

-- ^
-- >>> :t (:)
-- (:) :: a -> [a] -> [a]

-- ^
-- >>> :t (ccons)
-- (ccons) :: a -> [a] -> [a]

-- ^
-- >>> :t uncurry (ccons)
-- uncurry (ccons) :: (a, [a]) -> [a]


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



-- ** List Concatenation
cconcat = (++)
-- ^
-- Then, we'll also need to uncurry ('++'), which I've named "cconcat":

-- ^
-- >>> "this" ++ "word"
-- "thisword"

-- ^
-- >>> "this" `cconcat` "word"
-- "thisword"

-- ^
-- This has a familar type:
--

{- TODO: Fix this. Doctest doesn't support unicode!
-- >>> :t ⊙
-- (⊙) :: [a] -> [a] -> [a]

-}

-- ^
-- >>> :t cconcat
-- cconcat :: [a] -> [a] -> [a]

-- ^
-- >>> :t uncurry cconcat
-- uncurry cconcat :: ([a], [a]) -> [a]


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
