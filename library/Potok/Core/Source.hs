module Potok.Core.Source where

import Potok.Prelude
import qualified Potok.Core.Fetcher as A
import qualified Data.Attoparsec.Types as I
import qualified Data.Attoparsec.ByteString as K
import qualified Data.Attoparsec.Text as L


newtype Source element =
  Source (forall x. (A.Fetcher element -> IO x) -> IO x)

deriving instance Functor Source

instance Applicative Source where
  pure x =
    Source (\fetch -> fetch (pure x))
  (<*>) (Source leftIO) (Source rightIO) =
    Source (\fetch -> leftIO (\leftFetcher -> rightIO (\rightFetcher -> fetch (leftFetcher <*> rightFetcher))))

instance Monad Source where
  return = pure
  (>>=) (Source leftIO) rightK =
    Source $ \fetch ->
    leftIO $ \(A.Fetcher sendLeft) ->
    fetch $ 
    A.Fetcher $ \sendEnd sendRightElement ->
    sendLeft sendEnd $ \leftElement ->
    case rightK leftElement of
      Source rightIO ->
        rightIO $ \(A.Fetcher sendRight) ->
        sendRight sendEnd sendRightElement

{-# INLINE fetcher #-}
fetcher :: A.Fetcher element -> Source element
fetcher fetcher =
  Source (\fetch -> fetch fetcher)

{-# INLINE mapWithParseResult #-}
mapWithParseResult :: (input -> I.IResult input parsed) -> Source input -> Source (Either Text parsed)
mapWithParseResult inputToResult (Source inputIO) =
  Source $ \acquiredParsedFetcherHandler ->
  inputIO $ \inputFetcher ->
  do
    parsedFetcher <- A.mapWithParseResult inputToResult inputFetcher
    acquiredParsedFetcherHandler parsedFetcher

{-|
Lift an Attoparsec ByteString parser.

Consumption is non-greedy and terminates when the parser is done.
-}
{-# INLINE parseBytes #-}
parseBytes :: K.Parser parsed -> Source ByteString -> Source (Either Text parsed)
parseBytes parser =
  mapWithParseResult (K.parse parser)

{-|
Lift an Attoparsec Text parser.

Consumption is non-greedy and terminates when the parser is done.
-}
{-# INLINE parseText #-}
parseText :: L.Parser parsed -> Source Text -> Source (Either Text parsed)
parseText parser =
  mapWithParseResult (L.parse parser)
