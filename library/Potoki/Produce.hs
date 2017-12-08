module Potoki.Produce
(
  Produce,
  transform,
  list,
  vector,
  hashMapRows,
  fileBytes,
  fileBytesAtOffset,
  fileText,
  directoryContents,
  finiteMVar,
  infiniteMVar,
)
where

import Potoki.Prelude
import Potoki.Core.Produce
import qualified Potoki.Fetch as A
import qualified Potoki.Core.Fetch as A
import qualified Potoki.Core.Consume as E
import qualified Potoki.Core.Transform as F
import qualified Data.Attoparsec.Types as I
import qualified Data.Attoparsec.ByteString as K
import qualified Data.Attoparsec.Text as L
import qualified Data.HashMap.Strict as B
import qualified Data.ByteString as D
import qualified Data.Vector as C
import qualified System.Directory as G


{-# INLINE transform #-}
transform :: F.Transform input output -> Produce input -> Produce output
transform (F.Transform transformIO) (Produce produceIO) =
  Produce $ do
    (fetch, kill) <- produceIO
    newFetch <- transformIO fetch
    return (newFetch, kill)

{-# INLINE vector #-}
vector :: Vector input -> Produce input
vector vector =
  Produce $ do
    indexRef <- newIORef 0
    let
      fetch =
        A.Fetch $ \ nil just -> do
          index <- readIORef indexRef
          writeIORef indexRef $! succ index
          return $ case (C.!?) vector index of
            Just !input -> just input
            Nothing -> nil
      in return (fetch, return ())

{-# INLINE hashMapRows #-}
hashMapRows :: HashMap a b -> Produce (a, b)
hashMapRows =
  list . B.toList

{-|
Read from a file by path.

* Exception-free
* Automatic resource management
-}
{-# INLINABLE fileBytes #-}
fileBytes :: FilePath -> Produce (Either IOException ByteString)
fileBytes path =
  Produce $ do
    handleOpened <- try (openBinaryFile path ReadMode)
    case handleOpened of
      Right handle ->
        return (A.handleBytes handle ioChunkSize, catchIOError (hClose handle) (const (return ())))
      Left exception ->
        return (pure (Left exception), return ())

{-|
Read from a file by path.

* Exception-free
* Automatic resource management
-}
{-# INLINABLE fileBytesAtOffset #-}
fileBytesAtOffset :: FilePath -> Int -> Produce (Either IOException ByteString)
fileBytesAtOffset path offset =
  Produce (catchIOError success failure)
  where
    success =
      do
        handle <- openBinaryFile path ReadMode
        hSeek handle AbsoluteSeek (fromIntegral offset)
        return (A.handleBytes handle ioChunkSize, catchIOError (hClose handle) (const (return ())))
    failure exception =
      return (pure (Left exception), return ())

{-# INLINABLE directoryContents #-}
directoryContents :: FilePath -> Produce (Either IOException FilePath)
directoryContents path =
  Produce (catchIOError success failure)
  where
    success =
      do
        subPaths <- G.listDirectory path
        ref <- newIORef (map (Right . mappend path . (:) '/') subPaths)
        return (A.list ref, return ())
    failure exception =
      return (pure (Left exception), return ())

{-|
Read from a file by path.

* Exception-free
* Automatic resource management
-}
{-# INLINABLE fileText #-}
fileText :: FilePath -> Produce (Either IOException Text)
fileText path =
  Produce (catchIOError success failure)
  where
    success =
      do
        handle <- openBinaryFile path ReadMode
        return (A.handleText handle, catchIOError (hClose handle) (const (return ())))
    failure exception =
      return (pure (Left exception), return ())

{-|
Read from MVar.
Nothing gets interpreted as the end of input.
-}
{-# INLINE finiteMVar #-}
finiteMVar :: MVar (Maybe element) -> Produce element
finiteMVar var =
  Produce (return (A.finiteMVar var, return ()))

{-|
Read from MVar.
Never stops.
-}
{-# INLINE infiniteMVar #-}
infiniteMVar :: MVar element -> Produce element
infiniteMVar var =
  Produce (return (A.infiniteMVar var, return ()))
