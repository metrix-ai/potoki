module Potoki.Transform.Basic
where

import Potoki.Prelude hiding (take, takeWhile, filter, drop)
import Potoki.Core.Transform
import qualified Potoki.Fetch as A
import qualified Potoki.Core.Fetch as A
import qualified Data.HashSet as C
import qualified Data.Vector as P


{-# INLINE mapFilter #-}
mapFilter :: (input -> Maybe output) -> Transform input output
mapFilter mapping =
  Transform (pure . A.mapFilter mapping)

{-# INLINE filter #-}
filter :: (input -> Bool) -> Transform input input
filter predicate =
  Transform (pure . A.filter predicate)

{-# INLINE just #-}
just :: Transform (Maybe input) input
just =
  Transform (pure . A.just)

{-# INLINE takeWhile #-}
takeWhile :: (input -> Bool) -> Transform input input
takeWhile predicate =
  Transform (pure . A.takeWhile predicate)

{-# INLINE drop #-}
drop :: Int -> Transform input input
drop amount =
  Transform $ \ (A.Fetch fetchIO) -> do
    countRef <- newIORef amount
    return $ A.Fetch $ \ nil just -> fix $ \ loop -> do
      count <- readIORef countRef
      if count > 0
        then do
          writeIORef countRef $! pred count
          loop
        else fetchIO nil just

{-# INLINE list #-}
list :: Transform [a] a
list =
  Transform $ \ (A.Fetch fetchListIO) -> do
    bufferRef <- newIORef []
    return $ A.Fetch $ \ nil just -> do
      buffer <- readIORef bufferRef
      case buffer of
        head : tail -> do
          writeIORef bufferRef tail
          return (just head)
        _ ->
          let
            fetchElementIO =
              let
                nilIO =
                  return nil
                justIO input =
                  case input of
                    head : tail -> do
                      writeIORef bufferRef tail
                      return (just head)
                    _ -> do
                      writeIORef bufferRef []
                      return nil
                in join (fetchListIO nilIO justIO)
            in fetchElementIO

{-# INLINABLE vector #-}
vector :: Transform (Vector a) a
vector =
  Transform $ \ (A.Fetch fetchVectorIO) -> do
    indexRef <- newIORef 0
    vectorRef <- newIORef mempty
    return $ A.Fetch $ \ nil just -> fix $ \ loop -> do
      vector <- readIORef vectorRef
      index <- readIORef indexRef
      if index < P.length vector
        then do
          writeIORef indexRef (succ index)
          return (just (P.unsafeIndex vector index))
        else join $ fetchVectorIO (return nil) $ \ vector -> do
          writeIORef vectorRef vector
          writeIORef indexRef 0
          loop

{-# INLINE distinctBy #-}
distinctBy :: (Eq comparable, Hashable comparable) => (element -> comparable) -> Transform element element
distinctBy f =
  Transform $ \ (A.Fetch fetch) -> do
    stateRef <- newIORef mempty
    return $ A.Fetch $ \ nil just -> fix $ \ loop -> join $ fetch (return nil) $ \ !input -> do
      let comparable = f input
      !set <- readIORef stateRef
      if C.member comparable set
        then loop
        else do
          writeIORef stateRef $! C.insert comparable set
          return (just input)

{-# INLINE distinct #-}
distinct :: (Eq element, Hashable element) => Transform element element
distinct = distinctBy id

{-# INLINE mapInIO #-}
mapInIO :: (a -> IO b) -> Transform a b
mapInIO io =
  Transform $ \ (A.Fetch fetch) ->
  return $ A.Fetch $ \ nil just ->
  join $ fetch (return nil) $ (fmap . fmap) just io

{-# INLINE ioTransform #-}
ioTransform :: IO (Transform a b) -> Transform a b
ioTransform io =
  Transform $ \ fetch -> do
    Transform transformIO <- io
    transformIO fetch

{-|
Useful for debugging
-}
traceWithCounter :: (Int -> String) -> Transform a a
traceWithCounter show =
  ioTransform $ do
    counter <- newIORef 0
    return $ mapInIO $ \ x -> do
      n <- atomicModifyIORef' counter (\ n -> (succ n, n))
      putStrLn (show n)
      return x
