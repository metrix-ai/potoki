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


{-# INLINABLE handleBytes #-}
handleBytes :: Handle -> Int -> Fetch (Either IOException ByteString)
handleBytes handle chunkSize =
  Fetch $ \ nil just -> do
    element <- try (D.hGetSome handle chunkSize)
    case element of
      Right "" -> return nil
      _ -> return (just element)

{-# INLINABLE mapFilter #-}
mapFilter :: (input -> Maybe output) -> Fetch input -> Fetch output
mapFilter mapping (Fetch fetchIO) =
  Fetch $ \ nil just ->
  fix $ \ loop ->
  join $ fetchIO (return nil) $ \ input ->
  case mapping input of
    Just output -> return (just output)
    Nothing -> loop

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
