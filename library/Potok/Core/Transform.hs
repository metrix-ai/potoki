module Potok.Core.Transform where

import Potok.Prelude
import qualified Potok.Core.Fetch as A
import qualified Data.Attoparsec.ByteString as K
import qualified Data.Attoparsec.Text as L


newtype Transform input output =
  Transform (A.Fetch input -> IO (A.Fetch output))

instance Category Transform where
  id =
    Transform return
  (.) (Transform leftFetchIO) (Transform rightFetchIO) =
    Transform (leftFetchIO <=< rightFetchIO)

instance Profunctor Transform where
  dimap inputMapping outputMapping (Transform fetcherIO) =
    Transform (\inputFetch -> (fmap . fmap) outputMapping (fetcherIO (fmap inputMapping inputFetch)))

instance Strong Transform where
  first' (Transform io) =
    Transform (A.first io) 

instance Arrow Transform where
  arr fn =
    Transform (pure . fmap fn)
  first (Transform io) =
    Transform (A.first io)


{-|
Lift an Attoparsec ByteString parser.

Consumption is non-greedy and terminates when the parser is done.
-}
{-# INLINE parseBytes #-}
parseBytes :: K.Parser parsed -> Transform ByteString (Either Text parsed)
parseBytes parser =
  Transform (A.parseBytes parser)

{-|
Lift an Attoparsec Text parser.

Consumption is non-greedy and terminates when the parser is done.
-}
{-# INLINE parseText #-}
parseText :: L.Parser parsed -> Transform Text (Either Text parsed)
parseText parser =
  Transform (A.parseText parser)

take :: Int -> Transform input input
take =
  undefined

{-# INLINE mapFilter #-}
mapFilter :: (input -> Maybe output) -> Transform input output
mapFilter mapping =
  Transform (pure . A.mapFilter mapping)
