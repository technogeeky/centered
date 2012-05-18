
--
--   h  ( l ++ r ) =  h l  (-)  h r
--
--   h is a homomorphism
--   l
--   r are finite lists
--
--   Examples of list homomorphisms:
--
--        id             
--        map  f
--        concat
--        head
--        length         --        length ( x ++ y )   = legnth  x + length y        = hom (+) one 0 where one a = 1
--        sum            --        sum    ( x ++ y )   = sum     x + sum    y        = hom (+) id  0
--        min
--        all
--
--   Examples which are *not* list homomorpsims:
--
--        lsp            -- longest sorted prefix
--
--


--   The list function is (+)-leftwards for binary operator (+)
--        
--        iff  forall    elements  a.
--                       lists     y.
--
--        h ([a] ++ y) = a (+) h y
--
--        
--   For example, lsp is (+) leftwards:
--

lw a [   ] = [a]
lw a (b:x) |  a <= b    =  a : b : x
            |  otherwise =  [a]

--   Since:
--        lsp []    = []
--        lsp       = foldr (lw) []


--   foldr     (lw)      e  [ a1,a2,a3 ] = a1 `lw` ( a2 `lw` ( a3 `lw` e ) )
--
--   foldr     (lw)      e  (  x ++ y  ) = foldr (lw) ( foldr (lw) e y ) x
--
lsp = foldr (lw) [0] [1,2,3]

