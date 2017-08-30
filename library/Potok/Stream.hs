module Potok.Stream
(
  C.Stream,
  sink,
  C.parseBytes,
  C.parseText,
  C.take,
)
where

import Potok.Prelude
import qualified Potok.Core.Source as A
import qualified Potok.Core.Sink as B
import qualified Potok.Core.Stream as C
import qualified Potok.Core.Fetcher as D


{-# INLINE sink #-}
sink :: B.Sink input output -> C.Stream input output
sink (B.Sink sink) =
  C.Stream (D.sink sink)
