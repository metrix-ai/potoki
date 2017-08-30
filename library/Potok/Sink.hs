module Potok.Sink
(
  B.Sink,
  stream,
  B.head,
  B.list,
  B.reverseList,
)
where

import Potok.Prelude
import qualified Potok.Core.Source as A
import qualified Potok.Core.Sink as B
import qualified Potok.Core.Stream as C
import qualified Potok.Core.Fetcher as D


{-# INLINE stream #-}
stream :: C.Stream input output -> B.Sink output sinkOutput -> B.Sink input sinkOutput
stream (C.Stream stream) (B.Sink sink) =
  B.Sink (stream >=> sink)
