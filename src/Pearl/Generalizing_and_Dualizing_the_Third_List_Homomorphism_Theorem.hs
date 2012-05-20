{-# LANGUAGE ViewPatterns, PatternGuards #-}

-- * Introduction
--
-- Among the important 'parallel skeletons' in programming is the 'list homomorphism'.
--
-- >           h (xs ++ ys) = h xs `o` h ys

module Pearl.Generalizing_and_Dualizing_the_Third_List_Homomorphism_Theorem where



-- * The Third List-Homomorphism Theorem

-- | In the world of sets and total functions, the equations:
-- 
-- > (1)       h    [ ]  =   e                    -- where e :: b
-- > (1)       h  (x:xs) = (<||) (x, h xs)        -- where (<||) :: (a*b) -> b 
--
-- have a unique solution for @h :: [a] -> b@, denoted by:
-- 
-- >           h         = foldr fReverse e

-- |
-- >>> :t h []
-- h [] :: b
h :: [a] -> b
h = undefined

-- * Uncurrying for Point-Free
--
-- We deviate from standard and let fRev be uncurried for convenience in point-free style.

-- ** List constructor
ccons = (:)
-- ^
-- First, we uncurry the normal (curried) Haskell list constructor @(:)@, which I've named "ccons".
--
-- >>> :t (ccons)
-- (ccons) :: a -> [a] -> [a]
--
-- >>> :t uncurry (ccons)
-- uncurry (ccons) :: (a, [a]) -> [a]
--
-- to get "cons":
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
-- We also uncurry @(++)@, which I've named "cconcat":

-- ^
-- >>> :t cconcat
-- cconcat :: [a] -> [a] -> [a]

-- ^
-- >>> :t uncurry cconcat
-- uncurry cconcat :: ([a], [a]) -> [a]

-- ^
-- >>> "this" `cconcat` "word"
-- "thisword"

-- To get "cat":
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


-- |
-- Then, we define a product functor, which must satisfy a law:
--
-- > (f >< g) . (h >< k) = ((f . h) >< (g . k))
--
(><) :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
(f >< g) (x,y) = (f x, g y)


uncons         = undefined  -- not necessary (yet)


-- * Fold (uncurried)
foldrr (<||) ( []  ,  e ) = e
foldrr (<||) ( x:xs,  e ) = (<||) (  x  , foldrr (<||) (xs,e)  )

-- ^
-- We define a variation of foldr that takes the base case as an extra argument.
--
--   This can be seen as a "resumed" version of foldr.
--
--   That is:
--
-- >   (2)      
-- >            from:
-- >                           h            = foldr  (<||) e
-- >            show:
-- >                           h (xs ++ ys) = foldrr (<||) ( xs , h ys )
-- >            by:
-- >                           induction on xs





-- |
-- 
-- |
--   (3)       with:
--                            cat (xs,ys)    = xs ++ ys
--             equation (2):
--                            h (xs ++ ys)   = foldrr (<||) ( xs , h ys )
--             can be written:
--                            h . cat        = foldrr (<||) . ( id >< h )



-- |
--   (4)                      h . cat        = foldlr (||>) . (h >< id)
--
--
foldlr (||>) ( e   ,  []              ) = e
foldlr (||>) ( e   , unsnoc -> (xs,x) ) = (||>) ( foldlr (||>) (e,xs)  ,  x  )

snoc (xs,x) = xs ++ [x]

unsnoc :: [a] -> ([a],a)
unsnoc (l:r:[]) = ([l],r)
unsnoc ________ = error "i didn't write this correctly"


foldrr :: ((a,b) -> b) -> ([a], b  ) -> b
foldlr :: ((b,a) -> b) -> ( b , [a]) -> b





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

