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
    transformArrowLaws
    ,
    transformArrowChoiceLaws
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
      (first (arr f >>> arr g) :: A.Transform (Int, Char) (Int, Char))
      (first (arr f) >>> first (arr g))
    ,
    transformProperty "first f >>> arr fst = arr fst >>> f"
      (first (arr f) >>> arr fst :: A.Transform (Int, Char) Int)
      (arr fst >>> arr f)
    ,
    transformProperty "first f >>> arr (id *** g) = arr (id *** g) >>> first f"
      (first (arr f) >>> arr (id *** g))
      (arr (id *** g) >>> first (arr f))
    ,
    transformProperty "first (first f) >>> arr assoc = arr assoc >>> first f"
      (first (first (arr f)) >>> arr assoc :: A.Transform ((Int, Char), Double) (Int, (Char, Double)))
      (arr assoc >>> first (arr f))
  ]
  where
    f = (+24) :: Int -> Int
    g = (*3) :: Int -> Int
    assoc ((a,b),c) = (a,(b,c))

transformArrowChoiceLaws =
  testGroup "Transform ArrowChoice laws"
  [
    testCase "" $ do
      let
        list = [Right 'a', Right 'b', Left 0]
        transform = left (arr f) >>> left (arr g)
      result <- C.produceAndConsume (E.list list) (D.transform transform D.list)
      assertEqual "" [Right 'a', Right 'b', Left 72] result
    ,
    testCase "" $ do
      let
        list = [Right 'a', Right 'b', Left 0]
        transform = left (arr f >>> arr g)
      result <- C.produceAndConsume (E.list list) (D.transform transform D.list)
      assertEqual "" [Right 'a', Right 'b', Left 72] result
    ,
    transformProperty "left (arr f) = arr (left f)"
      (left (arr f) :: A.Transform (Either Int Char) (Either Int Char))
      (arr (left f))
    ,
    transformProperty "left (f >>> g) = left f >>> left g"
      (left (arr f >>> arr g) :: A.Transform (Either Int Char) (Either Int Char))
      (left (arr f) >>> left (arr g))
    ,
    transformProperty "f >>> arr Left = arr Left >>> left f"
      (arr f >>> arr Left :: A.Transform Int (Either Int Char))
      (arr Left >>> left (arr f))
    ,
    transformProperty "left f >>> arr (id +++ g) = arr (id +++ g) >>> left f"
      (left (arr f) >>> arr (id +++ g))
      (arr (id +++ g) >>> left (arr f))
    ,
    transformProperty "left (left f) >>> arr assocsum = arr assocsum >>> left f"
      (left (left (arr f)) >>> arr assocsum :: A.Transform (Either (Either Int Char) Double) (Either Int (Either Char Double)))
      (arr assocsum >>> left (arr f))
  ]
  where
    f = (+24) :: Int -> Int
    g = (*3) :: Int -> Int
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


