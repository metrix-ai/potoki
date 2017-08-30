module Potok.Produce
(
  A.Produce,
  transform,
  A.list,
  A.fileBytes,
)
where

import Potok.Prelude
import qualified Potok.Core.Produce as A
import qualified Potok.Core.Consume as B
import qualified Potok.Core.Transform as C
import qualified Potok.Core.Fetch as D


{-# INLINE transform #-}
transform :: C.Transform input output -> A.Produce input -> A.Produce output
transform (C.Transform transform) (A.Produce fetch) =
  A.Produce (\send -> fetch (\fetcher -> transform fetcher >>= send))
