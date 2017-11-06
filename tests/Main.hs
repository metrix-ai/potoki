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


main =
  defaultMain $
  testGroup "All tests" $
  [
    testCase "list to list" $ do
      result <- C.produceAndConsume (E.list [1,2,3]) (D.list)
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
            E.transform (A.map (either (const Nothing) Just) >>> A.just) $
            E.fileBytes "samples/1"
      result <- C.produceAndConsume produce (fmap F.length D.concat)
      assertEqual "" 17400 result
    ,
    testCase "Transform order" $ do
      let
        list = [Left 1, Left 2, Right 'z', Left 2, Right 'a', Left 1, Right 'b', Left 0, Right 'x', Left 4, Left 3]
        transform = left (A.consume (D.transform (A.take 2) D.sum))
      result <- C.produceAndConsume (E.list list) (D.transform transform D.list)
      assertEqual "" [Left 3, Right 'z'] result
    ,
    testCase "Transform choice 1" $ do
      let
        list = [Left 1, Left 2, Right 'z', Left 2, Right 'a', Left 1, Right 'b', Left 0, Right 'x', Left 4, Left 3]
        transform = left (A.consume D.sum)
      result <- C.produceAndConsume (E.list list) (D.transform transform D.list)
      assertEqual "" [Left 3, Right 'z'] result
    ,
    testCase "Transform choice 2" $ do
      let
        list = [Left 1, Left 2, Right 'z', Left 2, Right 'a', Left 1, Right 'b', Left 0, Right 'x', Left 4, Left 3]
        transform = right (A.consume D.list)
      result <- C.produceAndConsume (E.list list) (D.transform transform D.list)
      assertEqual "" [Left 1] result
    ,
    testCase "Transform choice 3" $ do
      let
        list = [Right 'z', Right 'a', Left 3, Right 'b', Left 0, Right 'x', Left 4, Left 3]
        transform = right (A.consume D.list)
      result <- C.produceAndConsume (E.list list) (D.transform transform D.list)
      assertEqual "" [Right "za",Left 3] result
    ,
    testCase "Transform interrupted order" $ do
      let
        list = [Left 1, Left 2, Right 'a']
        transform = left (A.consume (D.transform (A.take 3) D.sum))
      result <- C.produceAndConsume (E.list list) (D.transform transform D.list)
      assertEqual "" [Left 3, Right 'a'] result
    ,
    transformArrowLaws
    ,
    parsing
  ]

transformArrowLaws =
  testGroup "Transform Arrow laws"
  [
    transformProperty "arr id = id"
      (arr id :: A.Transform Int Int)
      id
    ,
    transformProperty "arr (f >>> g) = arr f >>> arr g"
      (arr (f >>> g))
      (arr f >>> arr g)
    ,
    transformProperty "first (arr f) = arr (first f)"
      (first (arr f) :: A.Transform (Int, Char) (Int, Char))
      (arr (first f))
    ,
    transformProperty "first (f >>> g) = first f >>> first g"
      (first (transform1 >>> transform2) :: A.Transform (Int, Char) (Int, Char))
      (first (transform1) >>> first (transform2))
    ,
    transformProperty "first f >>> arr fst = arr fst >>> f"
      (first transform1 >>> arr fst :: A.Transform (Int, Char) Int)
      (arr fst >>> transform1)
    ,
    transformProperty "first f >>> arr (id *** g) = arr (id *** g) >>> first f"
      (first transform1 >>> arr (id *** g))
      (arr (id *** g) >>> first transform1)
    ,
    transformProperty "first (first f) >>> arr assoc = arr assoc >>> first f"
      (first (first transform1) >>> arr assoc :: A.Transform ((Int, Char), Double) (Int, (Char, Double)))
      (arr assoc >>> first transform1)
    ,
    transformProperty "left (arr f) = arr (left f)"
      (left (arr f) :: A.Transform (Either Int Char) (Either Int Char))
      (arr (left f))
    ,
    transformProperty "left (f >>> g) = left f >>> left g"
      (left (transform1 >>> transform2) :: A.Transform (Either Int Char) (Either Int Char))
      (left (transform1) >>> left (transform2))
    ,
    transformProperty "f >>> arr Left = arr Left >>> left f"
      (transform1 >>> arr Left :: A.Transform Int (Either Int Char))
      (arr Left >>> left transform1)
    ,
    transformProperty "left f >>> arr (id +++ g) = arr (id +++ g) >>> left f"
      (left transform1 >>> arr (id +++ g))
      (arr (id +++ g) >>> left transform1)
    ,
    transformProperty "left (left f) >>> arr assocsum = arr assocsum >>> left f"
      (left (left transform1) >>> arr assocsum :: A.Transform (Either (Either Int Char) Double) (Either Int (Either Char Double)))
      (arr assocsum >>> left transform1)
    ,
    transformProperty "left (left (arr f)) >>> arr assocsum = arr assocsum >>> left (arr f)"
      (left (left (arr f)) >>> arr assocsum :: A.Transform (Either (Either Int Char) Double) (Either Int (Either Char Double)))
      (arr assocsum >>> left (arr f))
  ]
  where
    f = (+24) :: Int -> Int
    g = (*3) :: Int -> Int
    transform1 = A.consume (D.transform (A.take 3) D.sum) :: A.Transform Int Int
    transform2 = A.consume (D.transform (A.take 2) D.sum) :: A.Transform Int Int
    assoc ((a,b),c) = (a,(b,c))
    assocsum (Left (Left x)) = Left x
    assocsum (Left (Right y)) = Right (Left y)
    assocsum (Right z) = Right (Right z)

transformProperty :: 
  (Arbitrary input, Show input, Eq output, Show output) => 
  String -> A.Transform input output -> A.Transform input output -> TestTree
transformProperty name leftTransform rightTransform =
  testProperty name property
  where
    property list =
      transform leftTransform === transform rightTransform
      where
        transform transform =
          unsafePerformIO (C.produceAndConsume (E.list list) (D.transform transform D.list))

parsing :: TestTree
parsing =
  testGroup "Parsing"
  [
    testCase "Sample 1" $ do
      let parser = B.double <* B.char ','
          transform = A.map (either (const Nothing) Just) >>> A.just >>> A.mapWithBytesParser parser
          produce = E.transform transform (E.fileBytes "samples/1")
      result <- C.produceAndConsume produce D.count
      assertEqual "" 4350 result
    ,
    testCase "Sample 1 greedy" $ do
      let parser = B.sepBy B.double (B.char ',')
          transform = A.map (either (const Nothing) Just) >>> A.just >>> A.mapWithBytesParser parser
          produce = E.transform transform (E.fileBytes "samples/1")
      result <- C.produceAndConsume produce D.list
      assertEqual "" [Right 4350] (fmap (fmap length) result)
    ,
    testCase "Split chunk" $
    let
      produce = E.list ["1", "2", "3"]
      parser = B.anyChar
      transform = A.mapWithBytesParser parser >>> A.map (either (const Nothing) Just) >>> A.just
      consume = D.transform transform D.count
      in do
        assertEqual "" 3 =<< C.produceAndConsume produce consume
  ]
