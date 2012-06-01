------------------------------------------------------------------------------------------------
-- |
-- Module      :  Pearl.GaDtTLHT.Section05
-- Copyright   :  (c) Drew Day 2012
--                (c) Shin-Cheng Mu 2011
--                (c) Akimasa Morihata 2011
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Drew Day <drewday@gmail.com>
-- Stability   :  stable
-- Portability :  portable (ViewPatterns, PatternGuards)
--
------------------------------------------------------------------------------------------------
module Pearl.GaDtTLHT.Section05 where

-- | This is a reference to the "Foo" module.

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
test05 = id
-- ^
-- >>> 2 + 2
-- 4


-- |
-- As mentioned earlier, when looking at the proof of Theorem 2 in detail, one notices that further generalization
-- is possible. Indeed, the presence of @R . cat@ in Theorem 4 is superficial, and the theorem can still be
-- generalized.
--

theorem07 :: a
theorem07 = undefined
-- ^
-- @
-- R   = U . (id '><' S )    -- (RUS)
--     = V . (T  '><' id)    -- (RVT)
-- @
-- 
-- where
-- 
-- @
--     S . S° . S = S        -- (S invertable)
--     T . T° . T = T        -- (T invertable)
-- @
-- 
-- imply
-- 
-- @
-- R   = R . (T° '><' S°) . (T '><' S)
-- @


theorem07proof :: a
theorem07proof = undefined
-- ^
-- @
--                     R = U . (id x S)
--  { S invertable }     = U . (id x S) . (id x S°) . (id x S)
--  { RUS, in reverse }  = R            . (id x S°) . (id x S)
--  { RVT }              = V . (T x id) . (id x S°) . (id x S)
--  { T° T = id ? }      = V . (T x id) . (T° x S°) . (T x S)
--  { RVT, in reverse }  = R            . (T° x S°) . (T x S)
--
--  { done }           R = R . (T° x S°) . (T x S)
-- @
-- 
-- However, the conclusion of the theorem, that @R = R . (T° x S°) . (T x S)@, does not have much
-- structure hinting at how this theorem can be useful. The use cases we have found are when @S@ is a
-- sub-expression of @R@ (for example, to prove Corrolary 5, we used @R = k° . cat@ and @S = T = k°@.
-- We do not require @S = T@ in general), and we use Theorem 7 to establish recursive equations about @S@,
-- hoping to construct a terminating definition of @S@.

