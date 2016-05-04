module Test where

import Test.DocTest
import Test.QuickCheck
import Test.QuickCheck.Function

main = doctest ["Calc.hs"]
