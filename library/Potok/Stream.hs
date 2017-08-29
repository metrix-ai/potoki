module Potok.Stream where

import Potok.Prelude
import qualified Potok.Fetcher as A
import qualified Data.Attoparsec.ByteString as K
import qualified Data.Attoparsec.Text as L


newtype Stream input output =
  Stream (A.Fetcher input -> IO (A.Fetcher output))

instance Category Stream where
  id =
    Stream return
  (.) (Stream leftSourceUpdate) (Stream rightSourceUpdate) =
    Stream (leftSourceUpdate <=< rightSourceUpdate)

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
