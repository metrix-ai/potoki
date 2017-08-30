module Potok.IO where

import Potok.Prelude
import qualified Potok.Core.Source as A
import qualified Potok.Core.Sink as B
import qualified Potok.Core.Stream as C
import qualified Potok.Core.Fetcher as D


sinkSource :: A.Source input -> B.Sink input output -> IO output
sinkSource (A.Source fetch) (B.Sink sink) =
  fetch sink
