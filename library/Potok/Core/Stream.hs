module Potok.Core.Stream where

import Potok.Prelude
import qualified Potok.Core.Fetcher as A
import qualified Data.Attoparsec.ByteString as K
import qualified Data.Attoparsec.Text as L


newtype Stream input output =
  Stream (A.Fetcher input -> IO (A.Fetcher output))

instance Category Stream where
  id =
    Stream return
  (.) (Stream leftFetcherIO) (Stream rightFetcherIO) =
    Stream (leftFetcherIO <=< rightFetcherIO)

instance Profunctor Stream where
  dimap inputMapping outputMapping (Stream fetcherIO) =
    Stream (\inputFetcher -> (fmap . fmap) outputMapping (fetcherIO (fmap inputMapping inputFetcher)))

instance Strong Stream where
  first' (Stream io) =
    Stream (A.first io) 

instance Arrow Stream where
  arr fn =
    Stream (pure . fmap fn)
  first (Stream io) =
    Stream (A.first io)


{-|
Lift an Attoparsec ByteString parser.

Consumption is non-greedy and terminates when the parser is done.
-}
{-# INLINE parseBytes #-}
parseBytes :: K.Parser parsed -> Stream ByteString (Either Text parsed)
parseBytes parser =
  Stream (A.parseBytes parser)

{-|
Lift an Attoparsec Text parser.

Consumption is non-greedy and terminates when the parser is done.
-}
{-# INLINE parseText #-}
parseText :: L.Parser parsed -> Stream Text (Either Text parsed)
parseText parser =
  Stream (A.parseText parser)

take :: Int -> Stream input input
take =
  undefined
