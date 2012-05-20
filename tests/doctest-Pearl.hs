module Main where

import Test.DocTest

main :: IO ()
main = doctest [
                 "--optghc=-packageghc"
               , "--optghc=-isrc"
               , "--optghc=-idist/build/autogen/"
               , "--optghc=-optP-include"
               , "--optghc=-optPdist/build/autogen/cabal_macros.h"
               , "src/Pearl/Generalizing_and_Dualizing_the_Third_List_Homomorphism_Theorem/Pearl.hs"
               , "src/Pearl/Generalizing_and_Dualizing_the_Third_List_Homomorphism_Theorem/Section01.hs"
               , "src/Pearl/Generalizing_and_Dualizing_the_Third_List_Homomorphism_Theorem/Section02.hs"
               , "src/Pearl/Generalizing_and_Dualizing_the_Third_List_Homomorphism_Theorem/Section03.hs"
               , "src/Pearl/Generalizing_and_Dualizing_the_Third_List_Homomorphism_Theorem/Section04.hs"
               , "src/Pearl/Generalizing_and_Dualizing_the_Third_List_Homomorphism_Theorem/Section05.hs"
               , "src/Pearl/Generalizing_and_Dualizing_the_Third_List_Homomorphism_Theorem/Section06.hs"
               , "src/Pearl/Generalizing_and_Dualizing_the_Third_List_Homomorphism_Theorem/Ref.hs"
               ]

