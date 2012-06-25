{-# LANGUAGE UnicodeSyntax #-}

data Tree a = L a | N  ( Tree a ) a ( Tree a )    deriving (Read,Show)
data Z    a =       Nl            a ( Tree a )  
                  | Nr ( Tree a ) a               deriving (Read,Show)


type Cxt a = [ Z a ]

val ( L   n    ) = n
val ( N _ n _  ) = n

scan ( e, [ ])      = [ ]
scan ( e, xs )      = acc ( e, build xs )

acc ( e, L x )      = [ e ]
acc ( e, N t _ u )  = acc ( e, t ) ++ acc ( e +++ val t, u )




build [ x ]         = L x
build ( uncat -> (xs,ys) )  |  (xs /= [ ]) && (ys /= [ ])
                 = let t = build xs
                       u = build ys
                   in 
                      N t ( val t +++ val u ) u


fill :: ([Z a], Tree a) -> Tree a

fill ( [    ]     , t )  = t
fill ( Nl x u : xs, t )  = fill ( xs, N t x u )
fill ( Nr t x : xs, u )  = fill ( xs, N t x u )


u =                                     N ( L 5 ) 6 ( L 7 ) 
t = N (    N ( L 1 ) 2 ( L 3 )) 4 ( N ( N ( L 5 ) 6 ( L 7 )) 8 ( L 9 ) )
cx = [                                                    Nl 8 ( L 9 ) 
     ,Nr ( N ( L 1 ) 2 ( L 3 )) 4 ]



f :: Tree a -> b
f' :: Cxt a -> b
f = undefined
f' = undefined


-- ^
-- >>> fill (cx,u)
-- N (N (L 1) 2 (L 3)) 4 (N (N (L 5) 6 (L 7)) 8 (L 9))


a = f . fill
b = f
c = f'

(+++) = undefined
(uncat) = undefined

fL = undefined
g = undefined

p = undefined


k :: ( ?k' :: b -> Cxt a
     , ?g  :: b -> (b,b)
     )                    => b -> Tree a
k v                       | p v                   = L ( fL v )
                          | ( vR, vL ) <- ?g v    = fill (( ?k' vR ),( k vL ))
--                        where k' :: b -> Cxt a
--                              k' = undefined

unfL  :: ( b -> ( b, a, b ) , b -> a , b -> Bool ) -> b -> Tree a 
unfpL :: ( b -> ( b, a, b ) , b -> a,  b -> Bool ) -> b -> [( Cxt a, b )]


unfL  fs@ ( gL, fL, p ) v  | p v                   = L ( fL v )
                           | (v1,c,v2) <- gL v     = N ( unfL fs v1 ) c ( unfL fs v2 )


unfpL fs@ ( gL, fL, p ) v                          = iterL ([],v) 
 where
  iterL ( xs, v ) |                    p v = [( xs, v )]
                  | ( v1 , x, v2 ) <- gL v =  ( xs, v ) : iterL ( Nl                x ( unfL fs v2 ) : xs, v1 ) 
                                                       ++ iterL ( Nr ( unfL fs v1 ) x                : xs, v2 )



cxtpR gR v = iterR ( v, [ ])
     where
             iterR ( v, cx ) = ( v, cx ) :   [ y | ( v', lr ) <- gR v,
                                               y              <- iterR ( v', cx ++ [ up lr ])
                                             ]

up (Right (v_l, x )) = Nr   ( k v_l ) x
up (Left  ( x ,v_r)) = Nl             x ( k v_r )


-- unfpR ::  ( GR a b, b -> [( b, a )]) -> b -> [( b, Tree a )]

unfpR :: (?k'::b -> Cxt a, ?g::b -> (b, b)) =>   (t1 -> [ (t1 , Either (a, b) (b, a) ) ]   , t -> [ (t1, a) ] ) -> t -> [(t1,Tree a)]
cxtR  :: (?k'::b -> Cxt a, ?g::b -> (b, b)) =>   (t  -> [ (t  , Either (a, b) (b, a) ) ]) -> t -> [ [Z a]   ]
unfR  :: (?k'::b -> Cxt a, ?g::b -> (b, b)) =>   (t1 -> [ (t1 , Either (a, b) (b, a) ) ]   , t -> [ (t1, a) ] ) -> t -> [Tree a]

 
unfpR (gR,fR)  v = [ ( v'' , fill ( cx, L x) ) | ( v'  , x  )        <- fR       v
                                               , ( v'' , cx )  <- cxtpR gR       v'                ]
cxtR   gR      v = [ cx                        | ( v'  , cx )  <- cxtpR gR       v        
                                               ,                                    null ( p v' )  ]
unfR  (gR,fR)  v = [ t                         | ( v'  ,  t )  <- unfpR (gR,fR)  v
                                               ,                                    null ( p v' )  ]

