module Main where

import Prelude hiding (first, second)
import Control.Arrow
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Potoki.IO as C
import qualified Potoki.Consume as D
import qualified Potoki.Transform as A
import qualified Potoki.Produce as E
import qualified Data.Attoparsec.ByteString.Char8 as B
import qualified Data.ByteString as F
import qualified Data.Vector as G
import qualified System.Random as H


main =
  defaultMain $
  testGroup "All tests" $
  [
    testCase "vector to list" $ do
      result <- C.produceAndConsume (E.vector (G.fromList [1,2,3])) (D.list)
      assertEqual "" [1,2,3] result
    ,
    testCase "just" $ do
      result <- C.produceAndConsume (E.list [Just 1, Nothing, Just 2]) (D.transform A.just D.list)
      assertEqual "" [1,2] result
    ,
    testCase "transform,consume,take" $ do
      let
        transform = A.consume (D.transform (A.take 3) D.list)
        consume = D.transform transform D.list
        produceAndConsume list = C.produceAndConsume (E.list list) (consume)
      assertEqual "" [[1,2,3], [4,5,6], [7,8]] =<< produceAndConsume [1,2,3,4,5,6,7,8]
      assertEqual "" [[1,2,3], [4,5,6], [7,8,9]] =<< produceAndConsume [1,2,3,4,5,6,7,8,9]
      assertEqual "" [] =<< produceAndConsume ([] :: [Int])
    ,
    testCase "File reading" $ do
      let produce =
            E.transform (arr (either (const Nothing) Just) >>> A.just) $
            E.fileBytes "samples/1"
      result <- C.produceAndConsume produce (fmap F.length D.concat)
      assertEqual "" 17400 result
    ,
    transform
    ,
    parsing
  ]

transform :: TestTree
transform =
  testGroup "Transform" $
  [
    testCase "Order" $ do
      let
        list = [Left 1, Left 2, Right 'z', Left 2, Right 'a', Left 1, Right 'b', Left 0, Right 'x', Left 4, Left 3]
        transform = left (A.consume (D.transform (A.take 2) D.sum))
      result <- C.produceAndConsume (E.list list) (D.transform transform D.list)
      assertEqual "" [Left 3, Right 'z', Left 2, Right 'a', Left 1, Right 'b', Left 0, Right 'x', Left 7] result
    ,
    testCase "Interrupted order" $ do
      let
        list = [Left 1, Left 2, Right 'a']
        transform = left (A.consume (D.transform (A.take 3) D.sum))
      result <- C.produceAndConsume (E.list list) (D.transform transform D.list)
      assertEqual "" [Left 3, Right 'a'] result
    ,
    testCase "Distinct" $ do
      let
        list = [1,2,3,2,3,2,1,4,1] :: [Int]
      result <- C.produceAndConsume (E.list list) (D.transform A.distinct D.list)
      assertEqual "" [1,2,3,4] result
    ,
    testCase "Distinct By" $ do
      let
        list = [(1, ""),(2, ""),(3, ""),(2, ""),(3, ""),(2, ""),(1, ""),(4, ""),(1, "")] :: [(Int, String)]
      result <- C.produceAndConsume (E.list list) (D.transform (A.distinctBy fst) D.list)
      assertEqual "" [(1, ""),(2, ""),(3, ""),(4, "")] result
    ,
    testCase "Concurrently" $ do
      let
        list = [1..20000]
        produce = E.list list
        transform =
          A.concurrently 12 $
          arr (\ x -> H.randomRIO (0, 100) >>= threadDelay >> return x) >>>
          A.executeIO
        consume = D.transform transform D.list
      result <- C.produceAndConsume produce consume
      assertBool "Is dispersed" (list /= result)
      assertEqual "Contains no duplicates" 0 (length result - length (nub result))
      assertEqual "Equals the original once sorted" list (sort result)
  ]

parsing :: TestTree
parsing =
  testGroup "Parsing" $
  [
    testCase "Sample 1" $ do
      let parser = B.double <* B.char ','
          transform = arr (either (const Nothing) Just) >>> A.just >>> A.parseBytes parser
          produce = E.transform transform (E.fileBytes "samples/1")
      result <- C.produceAndConsume produce D.count
      assertEqual "" 4350 result
    ,
    testCase "Sample 1 greedy" $ do
      let parser = B.sepBy B.double (B.char ',')
          transform = arr (either (const Nothing) Just) >>> A.just >>> A.parseBytes parser
          produce = E.transform transform (E.fileBytes "samples/1")
      result <- C.produceAndConsume produce D.list
      assertEqual "" [Right 4350] (fmap (fmap length) result)
    ,
    testCase "Split chunk" $
    let
      produce = E.list ["1", "2", "3"]
      parser = B.anyChar
      transform = A.parseBytes parser >>> arr (either (const Nothing) Just) >>> A.just
      consume = D.transform transform D.count
      in do
        assertEqual "" 3 =<< C.produceAndConsume produce consume
  ]
