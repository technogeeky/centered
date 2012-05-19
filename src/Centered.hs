{-# LANGUAGE ViewPatterns #-}


-- |
--
-- Some standard functions on lists:

{-
(<||) :: (a,b) -> b
(<||) = snd

(||>) :: (b,a) -> b
(||>) = fst
-}



-- |
--
--   (1)       h    [ ]  =   e                    :: b
--             h  (x:xs) = (<||) (x, h xs)        
--                         (<||)                  :: (a*b) -> b
--   (1a)      h . cons  = (<||) . (id -*- h)

-- |
-- >>> :t h []
-- h [] :: b

h :: [a] -> b
h = undefined

-- |
-- >>> :t h cons
-- h . cons :: (a, [a]) -> c

cons (l,r)     = l  :  r
uncons         = undefined  -- not necessary (yet)



snoc (xs,x) = xs ++ [x]

unsnoc :: [a] -> ([a],a)
unsnoc (l:r:[]) = ([l],r)
unsnoc ________ = error "i didn't write this correctly"


foldrr :: ((a,b) -> b) -> ([a], b  ) -> b
foldlr :: ((b,a) -> b) -> ( b , [a]) -> b




foldrr (<||) ( []  ,  e               ) = e
foldrr (<||) ( x:xs,  e               ) = (<||) (  x  , foldrr (<||) (xs,e)  )

-- |
--   (2)       from:
--                            h            = foldr (<||) e
--             show:
--                            h (xs ++ ys) = foldrr (<||) ( xs , h ys )
--             by:
--                            induction on xs


cat (xs,ys) = xs ++ ys
-- |
--   (3)       with:
--                            cat (xs,ys)    = xs ++ ys
--             equation (2):
--                            h (xs ++ ys)   = foldrr (<||) ( xs , h ys )
--             can be written:
--                            h . cat        = foldrr (<||) . ( id -*- h )



-- |
--   (4)                      h . cat        = foldlr (||>) . (h -*- id)
--
--
foldlr (||>) ( e   ,  []              ) = e
foldlr (||>) ( e   , unsnoc -> (xs,x) ) = (||>) ( foldlr (||>) (e,xs)  ,  x  )






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
(-*-) :: (a -> c) -> (b -> d) -> (a,b) -> (c,d)
(f -*- g) (x,y) = (f x, g y)

(/\) :: (a -> b) -> (a -> c) -> a -> (b,c)
(f /\ g) x = (f x, g x)




h_sum [] = 0
h_sum [x] = (+) x
--h_sum (l ++ r) = f (h l, h r)


uncurry1 :: (a -> b)           -> (a)     -> b
uncurry2 :: (a -> b -> c)      -> (a,b)   -> c
uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d

uncurry1 f (x) = f x
uncurry2 f (x,y) = f x y
uncurry3 f (x,y,z) = f x y z

