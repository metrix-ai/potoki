module Potoki.Transform
(
  Transform,
  -- * Potoki integration
  consume,
  produce,
  -- * Basics
  ioTransform,
  take,
  takeWhile,
  mapFilter,
  just,
  distinct,
  builderChunks,
  executeIO,
  mapInIO,
  -- * Parsing
  parseBytes,
  parseText,
  -- * Concurrency
  bufferize,
  concurrently,
  -- * File IO
  deleteFile,
  appendBytesToFile,
)
where

import Potoki.Prelude hiding (take, takeWhile)
import Potoki.Core.Transform
import qualified Potoki.Fetch as A
import qualified Potoki.Core.Fetch as A
import qualified Potoki.Core.IO as G
import qualified Potoki.Core.Produce as H
import qualified Data.Attoparsec.ByteString as K
import qualified Data.Attoparsec.Text as L
import qualified Data.Attoparsec.Types as M
import qualified Data.HashSet as C
import qualified Data.ByteString.Builder as E
import qualified Data.ByteString.Lazy as F
import qualified Data.ByteString as J
import qualified System.Directory as I
import qualified Control.Concurrent.Chan.Unagi.Bounded as B


{-# INLINE mapFilter #-}
mapFilter :: (input -> Maybe output) -> Transform input output
mapFilter mapping =
  Transform (pure . A.mapFilter mapping)

{-# INLINE just #-}
just :: Transform (Maybe input) input
just =
  Transform (pure . A.just)

{-# INLINE takeWhile #-}
takeWhile :: (input -> Bool) -> Transform input input
takeWhile predicate =
  Transform (pure . A.takeWhile predicate)

{-# INLINE bufferize #-}
bufferize :: Int -> Transform element element
bufferize size =
  Transform $ \ (A.Fetch fetch) -> do
    (inChan, outChan) <- B.newChan size
    forkIO $ fix $ \ loop ->
      join $ fetch
        (B.writeChan inChan Nothing)
        (\ !element -> B.writeChan inChan (Just element) >> loop)
    return $ A.Fetch $ \ nil just -> fmap (maybe nil just) (B.readChan outChan)

{-|
Identity Transform, which ensures that the inputs are fetched synchronously.

Useful for concurrent transforms.
-}
{-# INLINABLE sync #-}
sync :: Transform a a
sync =
  Transform $ \ (A.Fetch fetch) -> do
    activeVar <- newMVar True
    return $ A.Fetch $ \ nil just -> do
      active <- takeMVar activeVar
      if active
        then join $ fetch
          (do
            putMVar activeVar False
            return nil)
          (\ !element -> do
            putMVar activeVar True
            return (just element))
        else do
          putMVar activeVar False
          return nil

{-|
Execute the transform on the specified amount of threads.
The order of the outputs produced is indiscriminate.
-}
{-# INLINABLE concurrently #-}
concurrently :: Int -> Transform input output -> Transform input output
concurrently workersAmount transform =
  sync >>>
  concurrentlyUnsafe workersAmount transform

{-# INLINE concurrentlyUnsafe #-}
concurrentlyUnsafe :: Int -> Transform input output -> Transform input output
concurrentlyUnsafe workersAmount (Transform syncTransformIO) =
  Transform $ \ fetch -> do
    outChan <- newEmptyMVar
    replicateM_ workersAmount $ forkIO $ do
      A.Fetch fetchIO <- syncTransformIO fetch
      fix $ \ loop -> join $ fetchIO
        (putMVar outChan Nothing)
        (\ !result -> putMVar outChan (Just result) >> loop)
    activeWorkersAmountVar <- newMVar workersAmount
    return $ A.Fetch $ \ nil just -> fix $ \ loop -> do
      activeWorkersAmount <- takeMVar activeWorkersAmountVar
      if activeWorkersAmount <= 0
        then return nil
        else do
          fetchResult <- takeMVar outChan
          case fetchResult of
            Just result -> do
              putMVar activeWorkersAmountVar activeWorkersAmount
              return (just result)
            Nothing -> do
              putMVar activeWorkersAmountVar (pred activeWorkersAmount)
              loop

{-# INLINE mapWithParseResult #-}
mapWithParseResult :: forall input parsed. (Monoid input, Eq input) => (input -> M.IResult input parsed) -> Transform input (Either Text parsed)
mapWithParseResult inputToResult =
  Transform $ \ inputFetch ->
  do
    unconsumedRef <- newIORef mempty
    finishedRef <- newIORef False
    return (A.Fetch (fetchParsed inputFetch finishedRef unconsumedRef))
  where
    fetchParsed :: A.Fetch input -> IORef Bool -> IORef input -> forall x. x -> (Either Text parsed -> x) -> IO x
    fetchParsed (A.Fetch inputFetchIO) finishedRef unconsumedRef nil just =
      do
        finished <- readIORef finishedRef
        if finished
          then return nil
          else do
            unconsumed <- readIORef unconsumedRef
            if unconsumed == mempty
              then
                join $ inputFetchIO
                  (return nil)
                  (\input -> do
                    if input == mempty
                      then return nil
                      else matchResult (inputToResult input))
              else do
                writeIORef unconsumedRef mempty
                matchResult (inputToResult unconsumed)
      where
        matchResult =
          \case
            M.Partial inputToResult ->
              consume inputToResult
            M.Done unconsumed parsed ->
              do
                writeIORef unconsumedRef unconsumed
                return (just (Right parsed))
            M.Fail unconsumed contexts message ->
              do
                writeIORef unconsumedRef unconsumed
                writeIORef finishedRef True
                return (just (Left resultMessage))
              where
                resultMessage =
                  if null contexts
                    then fromString message
                    else fromString (showString (intercalate " > " contexts) (showString ": " message))
        consume inputToResult =
          join $ inputFetchIO
            (do
              writeIORef finishedRef True
              matchResult (inputToResult mempty))
            (\input -> do
              when (input == mempty) (writeIORef finishedRef True)
              matchResult (inputToResult input))

{-|
Lift an Attoparsec ByteString parser.
-}
{-# INLINE parseBytes #-}
parseBytes :: K.Parser parsed -> Transform ByteString (Either Text parsed)
parseBytes parser =
  mapWithParseResult (K.parse parser)

{-|
Lift an Attoparsec Text parser.
-}
{-# INLINE parseText #-}
parseText :: L.Parser parsed -> Transform Text (Either Text parsed)
parseText parser =
  mapWithParseResult (L.parse parser)

{-# INLINE mapInIO #-}
mapInIO :: (a -> IO b) -> Transform a b
mapInIO io =
  Transform $ \ (A.Fetch fetch) ->
  return $ A.Fetch $ \ nil just ->
  join $ fetch (return nil) $ (fmap . fmap) just io

{-# INLINE deleteFile #-}
deleteFile :: Transform FilePath (Either IOException ())
deleteFile =
  mapInIO (try . I.removeFile)

{-# INLINE appendBytesToFile #-}
appendBytesToFile :: Transform (FilePath, ByteString) (Either IOException ())
appendBytesToFile =
  mapInIO $ \ (path, bytes) ->
  try $ 
  withFile path AppendMode $ \ handle -> 
  J.hPut handle bytes

{-# INLINE distinct #-}
distinct :: (Eq element, Hashable element) => Transform element element
distinct =
  Transform $ \ (A.Fetch fetch) -> do
    stateRef <- newIORef mempty
    return $ A.Fetch $ \ nil just -> fix $ \ loop -> join $ fetch (return nil) $ \ !input -> do
      !set <- readIORef stateRef
      if C.member input set
        then loop
        else do
          writeIORef stateRef $! C.insert input set
          return (just input)

{-# INLINE builderChunks #-}
builderChunks :: Transform E.Builder ByteString
builderChunks =
  produce (H.list . F.toChunks . E.toLazyByteString)

{-# INLINE ioTransform #-}
ioTransform :: IO (Transform a b) -> Transform a b
ioTransform io =
  Transform $ \ fetch -> do
    Transform transformIO <- io
    transformIO fetch
