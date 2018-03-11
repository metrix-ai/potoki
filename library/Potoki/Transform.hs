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
  filter,
  just,
  list,
  vector,
  distinctBy,
  distinct,
  builderChunks,
  executeIO,
  mapInIO,
  state,
  -- * Parsing
  parseBytes,
  parseText,
  -- * Concurrency
  N.bufferize,
  N.concurrently,
  N.async,
  -- * File IO
  deleteFile,
  appendBytesToFile,
  -- * Debugging
  traceWithCounter,
)
where

import Potoki.Prelude hiding (take, takeWhile, filter)
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
import qualified Data.Vector as P
import qualified System.Directory as I
import qualified Control.Concurrent.Chan.Unagi.Bounded as B
import qualified Control.Monad.Trans.State.Strict as O
import qualified Potoki.Transform.Concurrency as N


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

{-|
Notice that you can control the emission of output of each step
by producing a list of outputs and then composing the transform with
the "list" transform.
-}
{-# INLINE state #-}
state :: (a -> O.State s b) -> s -> Transform a b
state stateFn initialState =
  Transform $ \ (A.Fetch fetchIO) -> do
    stateRef <- newIORef initialState
    return $ A.Fetch $ \ nil just -> do
      let
        nilIO =
          return nil
        justIO input =
          do
            currentState <- readIORef stateRef
            case O.runState (stateFn input) currentState of
              (output, newState) -> do
                writeIORef stateRef newState
                return (just output)
        in join (fetchIO nilIO justIO)

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
