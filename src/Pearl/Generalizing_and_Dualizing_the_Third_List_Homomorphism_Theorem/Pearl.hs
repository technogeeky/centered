module Pearl.Generalizing_and_Dualizing_the_Third_List_Homomorphism_Theorem.Pearl 
     (
     -- * Section 3
          module Section03

     -- | This should link to "Ref"
    
     -- * Section 4
     ,    module Section04
     -- * Section 5
     ,    module Section05
     -- * Section 6
     ,    module Section06
     -- * Section 7

     ,    module Ref

     , test
     ) where


--import Pearl.Generalizing_and_Dualizing_the_Third_List_Homomorphism_Theorem.Section01 as X
--import Pearl.Generalizing_and_Dualizing_the_Third_List_Homomorphism_Theorem.Section02 as X
import Pearl.Generalizing_and_Dualizing_the_Third_List_Homomorphism_Theorem.Section03 as Section03
import Pearl.Generalizing_and_Dualizing_the_Third_List_Homomorphism_Theorem.Section04 as Section04
import Pearl.Generalizing_and_Dualizing_the_Third_List_Homomorphism_Theorem.Section05 as Section05
import Pearl.Generalizing_and_Dualizing_the_Third_List_Homomorphism_Theorem.Section06 as Section06
import Pearl.Generalizing_and_Dualizing_the_Third_List_Homomorphism_Theorem.Ref as Ref


-- | This is a reference to the "Ref" module.

-- | This is a bulleted list:
--
--     * first item
--     * second item

-- | This is an enumerated list:
--
--     (1) first item
--
--     2. second item

-- | This is an enumerated list:
--
--     (1) first item
--
--     2. second item

-- | This is a definition list:
--
--   [@foo@] The description of @foo@.
--
--   [@bar@] The description of @bar@.

-- This is a URL:

-- <http://www.google.com>


-- And this is an anchor:
--
-- #anchorBob#
--
test = id
-- ^
-- >>> 2 + 2
-- 4
