{- | 
    El contenedor Relation modela asociaciones dos elementos.
    Ofrece búsqueda eficiente por cualquiera de los dos elementos. 

    Es similar a Data.Map en que asocia llaves (k) con valores (v).
  
    A diferencia del contenedor Data.Map, un elemento 
    puede estar asociado más de una vez.   
    
    Los dos propósito fundamentales de esta estructura son
    
    1. Asociar elementos.
    2. Ofrecer eficiencia en búsquedas por cualquiera de los 
       dos elementos.
    
    Como no están implementados ni map ni fold, debe convertir
    la estructura en una lista para procesarla secuencialmente.
-}     

{-
    2012/05/17. DD. Translating to English. Shortening names.

    2009/11/09. LFL. Se corrige la definición de delete.

    2009/11/26. LFL. Construcción
-}


module Data.Relacion (

   -- * The @Relation@ type
                                   -- * El tipo @Relation@

   Relation ()  

   -- * Provided functionality:
                                   -- *  Funcionalidad provista:
 
   -- Inquiry 

 , size         --  Number of tuples in the relation.
 , null         --  Is the number of tuples 0?

   -- * Construction
 
 , empty        --  Make an empty relation.
 , fromList     --  Make a relation from a list.
 , singleton    --  Build a FIXME: unitary relation.

   -- ** Operaciones 

 , union        --  Una dos relaciones.
 , unions       --  Concatene una lista de relaciones.
 , insert       --  Inserte una tupla en la relation.
 , delete       --  Elimine una tupla de la relation.
   -- El conjunto con los valores asociados a un valor del domain.
 , lookupD     
   -- El conjunto con los valores asociados a un valor del range.
 , lookupR    
 , inD    --  Is the element in the domain?
 , inR    --  "   "     "    "   "  range?
 , member --  "   "     "    "      either?
 , notMember    
 
   -- ** Conversión

 , toList       --  Construya una lista de una relation.
   --  Extract the domain into a Set.
 , dom          
   --  Extract the range into a Set.
 , ran

   -- ** Utilitarios

 , compactSet --  Compact a Set of Maybes
   
 , (|$>) -- restringe range según subconjunto. PICA.
  
 , (<$|) -- restringe domain según subconjunto. PICA.

 , (<|)  -- restricción de domain. Z.

 , (|>)  -- restricción de range. z.

)

where

import           Prelude           hiding (null)
import qualified Data.Map     as M
import qualified Data.Set     as S
import           Data.Maybe        (isJust, fromJust, fromMaybe)


{-
   La implementación no usa S.Set (a,b) porque es necesario
   poder buscar un elemento sin conocer el otro.
   Con Set, la búsqueda es con la función member y hay que
   conocer ambos valores.

   Hay dos mapas que deben ser actualizados de manera coordinada.

   Siempre hay que tener cuidado con el conjunto asociado por
   la llave. Si hay una unión de relaciones, hay que aplicar
   unión al conjunto de valores.
   Si hay una resta hay que manipular en la forma el conjunto 
   de valores.

   Como es multi-mapa una llave k puede tener asociada
   un conjunto de valores v.
   No permitimos la asociación k con un conjunto vacío.
-}
data Relation a b  = Relation { domain ::  M.Map a (S.Set b)
                              , range   ::  M.Map b (S.Set a)
                              }

    deriving (Show, Eq, Ord)
    

-- * Funciones sobre relaciones


--  El tamaño es calculado usando el domain.
-- |  @size r@ devuelve la cantidad de tuplas en la relation.

size    ::  Relation a b -> Int
size r  =   M.fold ((+) . S.size) 0 (domain r)



-- | Construye una relation sin elementos.

empty   ::  Relation a b 
empty   =   Relation M.empty M.empty


  
-- |
-- La lista debe tener formato [(k1, v1), (k2, v2),..,(kn, vn)].

fromList    ::  (Ord a, Ord b) => [(a, b)] -> Relation a b
fromList xs =
    Relation 
        { domain =  M.fromListWith S.union $ snd2Set    xs
        , range   =  M.fromListWith S.union $ flipAndSet xs
        } 
    where  
       snd2Set    = map ( \(x,y) -> (x, S.singleton y) ) 
       flipAndSet = map ( \(x,y) -> (y, S.singleton x) )



toList   ::  Relation a b -> [(a,b)]
toList r =   concatMap
               ( \(x,y) -> zip (repeat x) (S.toList y) )
               ( M.toList . domain $ r)
  
  

-- | 
-- Construye una relation compuesta por la asociación de @x@ y @y@.

singleton      ::  a -> b -> Relation a b
singleton x y  =   Relation 
                     { domain = M.singleton x (S.singleton y) 
                     , range   = M.singleton y (S.singleton x)
                     }



-- | La relation que resulta de unir dos relaciones @r@ y @s@.

union ::  (Ord a, Ord b) 
      =>  Relation a b -> Relation a b -> Relation a b

union r s       =  
    Relation 
      { domain =  M.unionWith S.union (domain r) (domain s)
      , range   =  M.unionWith S.union (range   r) (range   s)
      }


---------------------------------------------------------------
{- Este fragmento proviene de:
    -- Module      :  Data.Map
    -- Copyright   :  (c) Daan Leijen 2002
    --                (c) Andriy Palamarchuk 2008
    -- License     :  BSD-style
    -- Maintainer  :  libraries@haskell.org
    -- Stability   :  provisional
    -- Portability :  portable
 -} 
foldlStrict         ::  (a -> b -> a) -> a -> [b] -> a
foldlStrict f z xs  =   case xs of
      []     -> z
      (x:xx) -> let z' = f z x in seq z' (foldlStrict f z' xx)
---------------------------------------------------------------



-- | Concatena una lista de relaciones en una sola relation.

unions       ::  (Ord a, Ord b) => [Relation a b] -> Relation a b

unions       =   foldlStrict union empty



-- | Inserta la asociación entre @ x @ y @ y @ en la relation @ r @

insert       ::  (Ord a, Ord b) 
             =>  a -> b -> Relation a b -> Relation a b

insert x y r =  -- r { domain = domain', range = range' } 
                Relation domain' range'
  where 
   domain'  =  M.insertWith S.union x (S.singleton y) (domain r)
   range'    =  M.insertWith S.union y (S.singleton x) (range   r)


{- 
   El borrado no es difícil pero es delicado.
   r = { domain {  (k1, {v1a, v3})
                 ,  (k2, {v2a})
                 ,  (k3, {v3b, v3})
                 }
       , range   {  (v1a, {k1}
                 ,  (v2a, {k2{
                 ,  (v3 , {k1, k3}
                 ,  (v3b, {k3}
                 }
      }

   Para borrar (k,v) de la relation haga:
    1. Trabajando sobre el domain:
       1a. Borre v del conjunto VS asociado con k.
       1b. Si VS es vacío, elimine k del domain.
    2. Trabajando sobre el range:
       2a. Borre k del conjunto VS asociado con v
       2b. Si VS es vacío, elimine v del range. 
         
-}

-- |  Remueve una asociación de la relation.
delete       ::  (Ord a, Ord b) 
             =>  a -> b -> Relation a b -> Relation a b

delete x y r  =  r { domain = domain', range = range' } 
   where 
   domain'   =  M.update (borrar y) x (domain r)
   range'     =  M.update (borrar x) y (range   r)
   borrar e s =  if  S.singleton e == s
                     then  Nothing
                     else  Just $ S.delete e s
  
-- | El conjunto de valores asociados a un valor del domain.

lookupD     ::  Ord a =>  a -> Relation a b -> Maybe (S.Set b)
lookupD x r =   M.lookup  x  (domain r)



-- | El conjunto de valores asociados a un valor del range.

lookupR     ::  Ord b =>  b -> Relation a b -> Maybe (S.Set a)
lookupR y r =   M.lookup  y  (range   r)



-- | True si el elemento @ x @ pertenece al domain de @ r @.

inD     ::  Ord a =>  a -> Relation a b -> Bool
inD x r =   isJust $ lookupD x r



-- | True si el elemento pertenece al range.

inR     ::  Ord b =>  b -> Relation a b -> Bool
inR y r =   isJust $ lookupR y r



-- | True si la relation está vacía.

-- Before 2010/11/09 null::Ord b =>  Relation a b -> Bool
null    ::  Relation a b -> Bool
null r  =   M.null $ domain r  



-- | True si la relation contiene la asociación @x@ y @y@

member       ::  (Ord a, Ord b) =>  a -> b -> Relation a b -> Bool
member x y r =   case lookupD x r of
                      Just s  ->  S.member y s
                      Nothing ->  False
    


-- | True si un par no pertenece a la relation

notMember       ::  (Ord a, Ord b) =>  a -> b -> Relation a b -> Bool
notMember x y r =   not $ member x y r



-- | Devuelve el domain de la relation como un conjunto.

dom            ::  Relation a b -> S.Set a
dom r          =   M.keysSet (domain r)



-- | Devuelve el range de la relation como un conjunto.

ran            ::  Relation a b -> S.Set b
ran r          =   M.keysSet (range   r)



{- |
    Compacta un conjunto de conjuntos cuyos valores que pueden ser 
    @Just (Set x)@ o @Nothing@.
    
    Los casos @Nothing@ son purgados.

    Es similar a @concat@.
-}
compactSet ::  Ord a => S.Set (Maybe (S.Set a)) -> S.Set a

compactSet =   S.fold ( S.union . fromMaybe S.empty ) S.empty



{- |
     Implementación primitiva para el operador de 
     selección a la izquierda o a la derecha. 
     
     PICA provee dos operadores |> y <|,
     respectivamente |$> y <$| en esta biblioteca, que trabajan
     sobre una Relation y OIS's. PICA expone los operadores
     definidos acá, para no romper con la abstracción del
     tipo de datos Relation y porque teniendo acceso a los
     componentes escondidos de Relation, es más eficiente
     la implementación de la operación de restricción.

    (a <$| b) r 

      se lee: por cada elemento @b@ del conjunto @B@,
              seleccione un elemento @a@ del conjunto @A@
              si @a@ está relacionado con @b@ en la relation @r@.

    (a |$> b) r

      se lee: por cada elemento @a@ del conjunto @A@, 
              seleccione un elemento @b@ del conjunto @B@
              si @a@ está relacionado con @b@ en la relation @r@.

    Con respecto a los operadores de restricción de domain
    y restricción de range del lenguaje Z que devuelven una relation,
    los descritos son diferentes y devuelven el domain o el range.
   

-}


(<$|)          ::  (Ord a, Ord b) 
               =>  S.Set a -> S.Set b -> Relation a b -> S.Set a

(as <$| bs) r  =   as `S.intersection` generarAS bs

    where  generarAS = compactSet . S.map (`lookupR` r) 
    
    -- Los sub-conjuntos del domain (a) asociados con cada b,
    -- tal que b en B y b está en el range de la relation.
    -- La expresión S.map retorna un conjunto de Either (S.Set a).


-- ( Caso a |> r b )

(|$>)          ::  (Ord a, Ord b) 
               =>  S.Set a -> S.Set b -> Relation a b -> S.Set b

(as |$> bs) r  =   bs `S.intersection`  generarBS as

    where  generarBS = compactSet . S.map (`lookupD` r) 



-- | Restricción de domain para una relation. Modelado como en z.

(<|) :: (Ord a, Ord b) => S.Set a -> Relation a b  -> Relation a b

s <| r  =  fromList $ concatMap
               ( \(x,y) -> zip (repeat x) (S.toList y) )
               ( M.toList domain' )
    where
    domain'  =  M.unions . map filter . S.toList $ s
    filter x =  M.filterWithKey (\k _ -> k == x) dr
    dr        =  domain r  -- just to memoize the value


-- | Restricción de range para una relation. Modelado como en z.

(|>) :: (Ord a, Ord b) => Relation a b -> S.Set b -> Relation a b

r |> t =  fromList $ concatMap
               ( \(x,y) -> zip (S.toList y) (repeat x) )
               ( M.toList range' )
    where
    range'    =  M.unions . map filter . S.toList $ t
    filter x =  M.filterWithKey (\k _ -> k == x) rr
    rr        =  range r   -- just to memoize the value


{- Note:
 
   As you have seen this implementation is expensive in terms
   of storage. Information is registered twice.
   For the operators |> and <| we follow a pattern used in
   the @fromList@ constructor and @toList@ flattener:
   It is enough to know one half of the Relation (the domain or
   the range) to create to other half.
   
-}


{- No implementadas

 
   filter :: (a -> b -> Bool) -> Relation a b -> Relation a b
   map
   difference

-}

--eof
