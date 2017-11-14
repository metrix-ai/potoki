module Potoki.Core.Produce where

import Potoki.Prelude
import qualified Potoki.Core.Fetch as A
import qualified Data.Attoparsec.Types as I
import qualified Data.Attoparsec.ByteString as K
import qualified Data.Attoparsec.Text as L
import qualified Data.HashMap.Strict as B


{-|
Active producer of elements with support for early termination.
-}
newtype Produce element =
  Produce (forall x. (A.Fetch element -> IO x) -> IO x)

deriving instance Functor Produce

instance Applicative Produce where
  pure x =
    Produce (\fetch -> fetch (pure x))
  (<*>) (Produce leftIO) (Produce rightIO) =
    Produce (\fetch -> leftIO (\leftFetch -> rightIO (\rightFetch -> fetch (leftFetch <*> rightFetch))))

instance Monad Produce where
  return = pure
  (>>=) (Produce leftIO) rightK =
    Produce $ \fetch ->
    leftIO $ \(A.Fetch sendLeft) ->
    fetch $ 
    A.Fetch $ \sendEnd sendRightElement ->
    sendLeft sendEnd $ \leftElement ->
    case rightK leftElement of
      Produce rightIO ->
        rightIO $ \(A.Fetch sendRight) ->
        sendRight sendEnd sendRightElement

instance Alternative Produce where
  empty =
    Produce (\fetch -> fetch empty)
  (<|>) (Produce leftIO) (Produce rightIO) =
    Produce (\fetch -> leftIO (\leftFetch -> rightIO (\rightFetch -> fetch (leftFetch <|> rightFetch))))

{-# INLINE fetcher #-}
fetcher :: A.Fetch element -> Produce element
fetcher fetcher =
  Produce (\fetch -> fetch fetcher)

{-|
Read from a file by path.

* Exception-free
* Automatic resource management
-}
{-# INLINABLE fileBytes #-}
fileBytes :: FilePath -> Produce (Either IOException ByteString)
fileBytes path =
  Produce $ \fetch -> do
    exceptionOrResult <- try $ withFile path ReadMode $ \handle -> fetch $ A.handleBytes handle chunkSize
    case exceptionOrResult of
      Left exception -> fetch (pure (Left exception))
      Right result -> return result
  where
    chunkSize =
      shiftL 2 12

{-|
Read from a file by path.

* Exception-free
* Automatic resource management
-}
{-# INLINABLE fileBytesAtOffset #-}
fileBytesAtOffset :: FilePath -> Int -> Produce (Either IOException ByteString)
fileBytesAtOffset path offset =
  Produce $ \fetch -> do
    exceptionOrResult <- try $ withFile path ReadMode $ \handle -> do
      hSeek handle AbsoluteSeek (fromIntegral offset)
      fetch $ A.handleBytes handle chunkSize
    case exceptionOrResult of
      Left exception -> fetch (pure (Left exception))
      Right result -> return result
  where
    chunkSize =
      shiftL 2 12

{-# INLINE list #-}
list :: [input] -> Produce input
list list =
  Produce (\fetch -> newIORef list >>= fetch . A.list)

{-# INLINE hashMapRows #-}
hashMapRows :: HashMap a b -> Produce (a, b)
hashMapRows =
  list . B.toList
