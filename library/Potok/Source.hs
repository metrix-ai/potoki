module Potok.Source where

import Potok.Prelude
import qualified Potok.Fetcher as A
import qualified Data.Attoparsec.Types as I
import qualified Data.Attoparsec.ByteString as K
import qualified Data.Attoparsec.Text as L


newtype Source element =
  Source (forall x. (A.Fetcher element -> IO x) -> IO x)

deriving instance Functor Source


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
