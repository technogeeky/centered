{-# LANGUAGE ViewPatterns #-}


-- |
--
-- Some standard functions on lists:
unsnoc :: [a] -> ([a],a)

cons (l,r) = l  :  r
snoc (l,r) = l ++ [r]
unsnoc (x:y:[]) = ([x],y)
cat  (l,r) = l ++  r

{-
(<||) :: (a,b) -> b
(<||) = snd

(||>) :: (b,a) -> b
(||>) = fst
-}

-- |
--
--   (1)       h    [ ]  =   e
--             h  (x:xs) = (<||) (x, h xs)
--
--   (1a)      h . cons  = (<||) . (id * h)


foldrr :: ((a,b) -> b) -> ([a], b  ) -> b
foldlr :: ((b,a) -> b) -> ( b , [a]) -> b

unfoldr :: (b -> (a,b)) -> (b -> Bool) -> b -> [a]
unfoldl :: (b -> (b,a)) -> (b -> Bool) -> b -> [a]

unfoldrp :: (b -> (a,b)) -> (b -> Bool) -> b -> [([a], b)]
unfoldlp :: (b -> (b,a)) -> (b -> Bool) -> b -> [(b, [a])]


foldrr (<||) ( []  ,  e               ) = e
foldrr (<||) ( x:xs,  e               ) = (<||) (  x  , foldrr (<||) (xs,e)  )
foldlr (||>) ( e   ,  []              ) = e
foldlr (||>) ( e   , unsnoc -> (xs,x) ) = (||>) ( foldlr (||>) (e,xs)  ,  x  )


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


unhom g f p q = k
     where 
     k v | p v = []
         | q v = [f v]
         | (l,r) <- g v = k l ++ k r




h_sum [] = 0
h_sum [x] = (+) x
--h_sum (l ++ r) = f (h l, h r)



