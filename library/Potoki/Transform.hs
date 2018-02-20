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
  distinctBy,
  distinct,
  builderChunks,
  executeIO,
  mapInIO,
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
import qualified System.Directory as I
import qualified Control.Concurrent.Chan.Unagi.Bounded as B
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
