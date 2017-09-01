module Potoki.Consume
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

import Potoki.Prelude
import qualified Potoki.Core.Produce as A
import qualified Potoki.Core.Consume as B
import qualified Potoki.Core.Transform as C
import qualified Potoki.Core.Fetch as D


{-# INLINE transform #-}
transform :: C.Transform input output -> B.Consume output sinkOutput -> B.Consume input sinkOutput
transform (C.Transform transform) (B.Consume sink) =
  B.Consume (transform >=> sink)
