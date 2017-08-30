module Potok.Source
(
  A.Source,
  stream,
  A.fileBytes,
)
where

import Potok.Prelude
import qualified Potok.Core.Source as A
import qualified Potok.Core.Sink as B
import qualified Potok.Core.Stream as C
import qualified Potok.Core.Fetcher as D


{-# INLINE stream #-}
stream :: C.Stream input output -> A.Source input -> A.Source output
stream (C.Stream stream) (A.Source fetch) =
  A.Source (\send -> fetch (\fetcher -> stream fetcher >>= send))
