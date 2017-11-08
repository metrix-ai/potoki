module Potoki.Transform
(
  C.Transform,
  consume,
  C.parseBytes,
  C.parseText,
  C.map,
  C.mapFilter,
  C.just,
  C.take,
  C.takeWhile,
  C.takeWhileIsJust,
  C.takeWhileIsLeft,
  C.takeWhileIsRight,
  C.bufferize,
  C.executeIO,
  C.deleteFile,
  C.appendBytesToFile,
)
where

import Potoki.Prelude
import qualified Potoki.Core.Produce as A
import qualified Potoki.Core.Consume as B
import qualified Potoki.Core.Transform as C
import qualified Potoki.Core.Fetch as D


{-# INLINE consume #-}
consume :: B.Consume input output -> C.Transform input output
consume (B.Consume consume) =
  C.consume consume
