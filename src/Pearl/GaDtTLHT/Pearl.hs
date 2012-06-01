------------------------------------------------------------------------------------------------
-- |
-- Module      :  Pearl.GaDtTLHT.Section03
-- Copyright   :  (c) Drew Day 2012
--                (c) Shin-Cheng Mu 2011
--                (c) Akimasa Morihata 2011
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Drew Day <drewday@gmail.com>
-- Stability   :  stable
-- Portability :  portable (ViewPatterns, PatternGuards)
--
------------------------------------------------------------------------------------------------
module Pearl.GaDtTLHT.Pearl 

     ( module Pearl.GaDtTLHT.Section01
     , module Pearl.GaDtTLHT.Section02
     , module Pearl.GaDtTLHT.Section03
     , module Pearl.GaDtTLHT.Section04
     , module Pearl.GaDtTLHT.Section05
     , module Pearl.GaDtTLHT.Section06
     , module Pearl.GaDtTLHT.Ref
     , test
     ) where


import Pearl.GaDtTLHT.Section01 hiding (cons)
import Pearl.GaDtTLHT.Section02 hiding (cat,h)
import Pearl.GaDtTLHT.Section03 hiding ((||>), (<||))
import Pearl.GaDtTLHT.Section04
import Pearl.GaDtTLHT.Section05 
import Pearl.GaDtTLHT.Section06 
import Pearl.GaDtTLHT.Ref 


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
