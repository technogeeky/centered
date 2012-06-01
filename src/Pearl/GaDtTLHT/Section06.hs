------------------------------------------------------------------------------------------------
-- |
-- Module      :  Pearl.GaDtTLHT.Section06
-- Copyright   :  (c) Drew Day 2012
--                (c) Shin-Cheng Mu 2011
--                (c) Akimasa Morihata 2011
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Drew Day <drewday@gmail.com>
-- Stability   :  stable
-- Portability :  portable (ViewPatterns, PatternGuards)
--
------------------------------------------------------------------------------------------------
module Pearl.GaDtTLHT.Section06 where

-- * Conclusions

conclusions :: a
conclusions = undefined
-- ^
-- By formulating the third list homomorphism theorem in point-free,
-- relational style, we have dualised the theorem to unfolds, as well as
-- generalised the theorem to both folds and unfolds for trees. While
-- the original theorem establishes a connection between insertion
-- sort and merge sort, the dual theorem shows a similar connection
-- between selection sort and quicksort. We have also derived an
-- efficient parallel algorithm for scan based on unfolds. To the best
-- of the authors' knowledge, while there have been many studies
-- on parallel programming based on structural recursion, none have
-- considered the dual -- list generation in the form of /unhom/.
--
-- The theorem generalises nicely to trees: if a function processes
-- or generates a tree both downwards and upwards, it may process or
-- generate the tree from the middle. Finally, we have also presented
-- an example that shows how a relational view may shed new light
-- on an old topic by revealing its hidden symmetry. The authors
-- believe that relational methods deserve to be appreciated more
-- among functional programmers.
-- 
-- As a remark, in practice, for both list and tree generation one
-- might need different types of seeds for left vs. right or inwards vs. outwards
-- unfolding. 
--
-- That is, we have 
--
-- @
--        k = unfoldr g_fwd p . i_1 
--          = unfoldl g_rev p . i_2
-- @
--
-- or 
--
-- @
--        k = unf_d   fs_d . i_1 
--          = unf_u   fs_u . i_2
-- @ 
--
-- for some i_1 and i_2 that initialise the seeds. 
--
-- This is also covered by Theorem 7. We leave it to the
-- readers to work out the details.

-- * Acknowledgements

acknowledgements :: a
acknowledgements = undefined
-- ^
-- The authors would like to thank Jeremy Gibbons, Jose Nuno Oliveira, and Janis Voigtlander for 
-- comments on earlier drafts, and to the anonymous referees for their valuable comments. This pearl
-- was inspired by a question proposed by Zhenjiang Hu, who also cooperated with the authors
-- throughout the development of this paper and made plenty of important technical contributions.
-- 
