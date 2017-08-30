module Potok.Core.Produce where

import Potok.Prelude
import qualified Potok.Core.Fetch as A
import qualified Data.Attoparsec.Types as I
import qualified Data.Attoparsec.ByteString as K
import qualified Data.Attoparsec.Text as L


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

{-# INLINE mapWithParseResult #-}
mapWithParseResult :: (input -> I.IResult input parsed) -> Produce input -> Produce (Either Text parsed)
mapWithParseResult inputToResult (Produce inputIO) =
  Produce $ \acquiredParsedFetchHandler ->
  inputIO $ \inputFetch ->
  do
    parsedFetch <- A.mapWithParseResult inputToResult inputFetch
    acquiredParsedFetchHandler parsedFetch

{-|
Lift an Attoparsec ByteString parser.

Consumption is non-greedy and terminates when the parser is done.
-}
{-# INLINE parseBytes #-}
parseBytes :: K.Parser parsed -> Produce ByteString -> Produce (Either Text parsed)
parseBytes parser =
  mapWithParseResult (K.parse parser)

{-|
Lift an Attoparsec Text parser.

Consumption is non-greedy and terminates when the parser is done.
-}
{-# INLINE parseText #-}
parseText :: L.Parser parsed -> Produce Text -> Produce (Either Text parsed)
parseText parser =
  mapWithParseResult (L.parse parser)

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

list :: [input] -> Produce input
list list =
  Produce (\fetch -> A.list list >>= fetch)
