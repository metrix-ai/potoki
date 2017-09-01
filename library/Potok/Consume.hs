module Potok.Consume
(
  B.Consume,
  transform,
  B.count,
  B.head,
  B.list,
  B.reverseList,
  B.concat,
  B.sum,
  B.writeBytesToFile,
  B.print,
)
where

import Potok.Prelude
import qualified Potok.Core.Produce as A
import qualified Potok.Core.Consume as B
import qualified Potok.Core.Transform as C
import qualified Potok.Core.Fetch as D


{-# INLINE transform #-}
transform :: C.Transform input output -> B.Consume output sinkOutput -> B.Consume input sinkOutput
transform (C.Transform transform) (B.Consume sink) =
  B.Consume (transform >=> sink)
