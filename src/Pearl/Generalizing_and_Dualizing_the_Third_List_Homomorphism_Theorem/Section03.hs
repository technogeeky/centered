{-# LANGUAGE ViewPatterns, PatternGuards #-}
module Pearl.Generalizing_and_Dualizing_the_Third_List_Homomorphism_Theorem.Section03 where

(<||) (x, v) = f (k x,   v)
     where k :: b -> [a]
           k = undefined
           f = undefined
(||>) (v, x) = f (  v, k x)
     where
          k :: b -> [a]
          k = undefined
          f = undefined

-- |
-- The function 'unfoldr', a dual of 'foldr' that generates a list from left
-- to right, may be defined as follows: ['note01']
-- 
-- @
-- unfoldr (||>)                  p   v 
--                |               p   v     = []
--                | (x, v') \<- (||>) v     = x : unfoldr (||>) p v'
-- @
--
unfoldr :: (b -> (a,b)) -> (b -> Bool) -> b -> [a]
unfoldr (||>)    p v 
               | p v                    = []
               | (x, v') <- (||>) v     = x : unfoldr (||>) p v'

-- |
-- Symmetrically, the function unfoldl is defined by:
-- 
-- @
-- unfoldl (<||)                   p    v
--                |                p    v     = []
--                | (v', x) \<- ('<||') v     = unfoldl (<||) p v' ++ [x]
-- @
unfoldl :: (b -> (b,a)) -> (b -> Bool) -> b -> [a]
unfoldl (<||)    p v
               | p v                    = []
               | (v', x) <- (<||) v     = unfoldl (<||) p v' ++ [x]


-- |
-- Typically, unfolds are defined for /coinductive, possibly infinite lists/. Since we want the unfolded lists to have both a left end
-- and a right end, and for another important technical reason to be mentioned later, our "unfolds" in this pearl return inductive, finite
-- lists and require separate proofs that all successive applications of '<||' and '||>' eventually reach some @v@ for which @p v@ is 'True'.
-- Due to space constraints, however, the proof of termination is usually treated informally.
-- 
-- 
-- Finally, we denote a function @k ∶∶ b → [ a ]@ by @'unhom' g f p q@ it it satisfies:
-- 
-- @
-- unhom g f p q = k
--      where 
--      k v | p v = []
--          | q v = [f v]
--          | (l,r) <- g v = k l ++ k r
-- @
-- 
-- 
unhom g f p q = k
     where 
     k v | p v = []
         | q v = [f v]
         | (l,r) <- g v = k l ++ k r
-- ^
-- We also demand that successive applications of g eventually produce seeds for which either p or q is true, and thus k generates finite
-- lists.





unfoldrp :: (b -> (a,b)) -> (b -> Bool) -> b -> [([a], b)]
unfoldlp :: (b -> (b,a)) -> (b -> Bool) -> b -> [(b, [a])]



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


