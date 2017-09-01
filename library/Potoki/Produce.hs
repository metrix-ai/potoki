module Potoki.Produce
(
  A.Produce,
  transform,
  A.list,
  A.fileBytes,
)
where

import Potoki.Prelude
import qualified Potoki.Core.Produce as A
import qualified Potoki.Core.Consume as B
import qualified Potoki.Core.Transform as C
import qualified Potoki.Core.Fetch as D


{-# INLINE transform #-}
transform :: C.Transform input output -> A.Produce input -> A.Produce output
transform (C.Transform transform) (A.Produce fetch) =
  A.Produce (\send -> fetch (\fetcher -> transform fetcher >>= send))
