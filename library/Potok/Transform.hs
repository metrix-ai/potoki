module Potok.Transform
(
  C.Transform,
  consume,
  C.parseBytes,
  C.parseText,
  C.mapFilter,
  C.take,
  C.takeWhileIsJust,
)
where

import Potok.Prelude
import qualified Potok.Core.Produce as A
import qualified Potok.Core.Consume as B
import qualified Potok.Core.Transform as C
import qualified Potok.Core.Fetch as D


{-# INLINE consume #-}
consume :: B.Consume input output -> C.Transform input output
consume (B.Consume consume) =
  C.Transform (pure . D.consume consume)
