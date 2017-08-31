module Main where

import Prelude hiding (first, second)
import Control.Arrow
import Test.QuickCheck.Instances
import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import qualified Potok.IO as C
import qualified Potok.Consume as D
import qualified Potok.Transform as A
import qualified Potok.Produce as E
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
    testCase "mapFilter" $
    assertEqual "" [1,2,3] =<< 
    C.produceAndConsume (E.list [1,5,2,3]) (D.transform (A.mapFilter (\x -> if x < 5 then Just x else Nothing)) D.list)
    ,
    testCase "File reading" $ do
      let produce =
            E.transform (A.mapFilter (either (const Nothing) Just)) $
            E.fileBytes "samples/1"
      result <- C.produceAndConsume produce (fmap F.length D.concat)
      assertEqual "" 3480 result
    ,
    testCase "Sample 1 parsing" $ do
      let parser = B.sepBy B.double (B.char ',')
          transform = A.mapFilter (either (const Nothing) Just) >>> A.parseBytes parser
          produce = E.transform transform (E.fileBytes "samples/1")
      result <- C.produceAndConsume produce D.count
      assertEqual "" 780 result
    ,
    transformArrowLaws
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
    testCase "Order" $ do
      let
        list = [Left 1, Right 'z', Left 2, Right 'a', Left 1, Right 'b', Left 0, Right 'x', Left 4, Left 3]
        transform = left transform2
      result <- C.produceAndConsume (E.list list) (D.transform transform D.list)
      assertEqual "" [Right 'z', Left 3, Right 'a', Right 'b', Left 1, Right 'x', Left 7] result
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


