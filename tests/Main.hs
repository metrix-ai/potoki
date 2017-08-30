module Main where

import Prelude
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Potok.IO as C
import qualified Potok.Consume as D
import qualified Potok.Transform as A
import qualified Potok.Produce as E


main =
  defaultMain $
  testGroup "All tests" $
  [
    testCase "" $
    do
      result <- C.produceAndConsume (E.list [1,2,3]) (D.list)
      assertEqual "" [1,2,3] result
  ]
