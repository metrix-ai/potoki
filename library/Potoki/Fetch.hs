module Potoki.Fetch
where

import Potoki.Prelude
import Potoki.Core.Fetch
import qualified Data.Attoparsec.Types as I
import qualified Data.Attoparsec.ByteString as K
import qualified Data.Attoparsec.Text as L
import qualified Data.HashMap.Strict as B
import qualified Data.Vector as C
import qualified Data.ByteString as D
import qualified Data.Text as A
import qualified Data.Text.IO as A


{-# INLINABLE handleBytes #-}
handleBytes :: Handle -> Fetch (Either IOException ByteString)
handleBytes =
  handleBytesWithChunkSize ioChunkSize

{-# INLINABLE handleBytesWithChunkSize #-}
handleBytesWithChunkSize :: Int -> Handle -> Fetch (Either IOException ByteString)
handleBytesWithChunkSize chunkSize handle =
  Fetch $ \ nil just -> do
    chunk <- try (D.hGetSome handle chunkSize)
    case chunk of
      Right "" -> return nil
      _ -> return (just chunk)

{-# INLINABLE handleText #-}
handleText :: Handle -> Fetch (Either IOException Text)
handleText handle =
  Fetch $ \ nil just -> do
    chunk <- try (A.hGetChunk handle)
    case chunk of
      Right "" -> return nil
      _ -> return (just chunk)

{-# INLINABLE mapFilter #-}
mapFilter :: (input -> Maybe output) -> Fetch input -> Fetch output
mapFilter mapping (Fetch fetchIO) =
  Fetch $ \ nil just ->
  fix $ \ loop ->
  join $ fetchIO (return nil) $ \ input ->
  case mapping input of
    Just output -> return (just output)
    Nothing -> loop

{-# INLINABLE filter #-}
filter :: (input -> Bool) -> Fetch input -> Fetch input
filter predicate (Fetch fetchIO) =
  Fetch $ \ nil just ->
  fix $ \ loop ->
  join $ fetchIO (return nil) $ \ input ->
  if predicate input
    then return (just input)
    else loop

{-# INLINABLE just #-}
just :: Fetch (Maybe element) -> Fetch element
just (Fetch fetchIO) =
  Fetch $ \ nil just ->
  fix $ \ loop ->
  join $ fetchIO (return nil) $ \ case
    Just output -> return (just output)
    Nothing -> loop

{-# INLINABLE takeWhile #-}
takeWhile :: (element -> Bool) -> Fetch element -> Fetch element
takeWhile predicate (Fetch fetchIO) =
  Fetch $ \ nil just ->
  fetchIO nil $ \ input ->
  if predicate input
    then just input
    else nil

{-# INLINABLE infiniteMVar #-}
infiniteMVar :: MVar element -> Fetch element
infiniteMVar var =
  Fetch $ \ nil just ->
  fmap just (takeMVar var)

{-# INLINABLE finiteMVar #-}
finiteMVar :: MVar (Maybe element) -> Fetch element
finiteMVar var =
  Fetch $ \ nil just ->
  fmap (maybe nil just) (takeMVar var)

{-# INLINABLE vector #-}
vector :: IORef Int -> Vector element -> Fetch element
vector indexRef vector =
  Fetch $ \ nil just -> do
    index <- readIORef indexRef
    if index < C.length vector
      then do
        writeIORef indexRef (succ index)
        return (just (C.unsafeIndex vector index))
      else return nil
