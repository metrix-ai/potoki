module Potoki.Transform.ByteString
where

import Potoki.Prelude
import Potoki.Core.Transform
import Potoki.Transform.State
import qualified Potoki.Fetch as A
import qualified Potoki.Core.Fetch as A
import qualified Potoki.Core.Produce as H
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as E
import qualified Data.ByteString.Lazy as F


{-# INLINE builderChunks #-}
builderChunks :: Transform E.Builder ByteString
builderChunks =
  produce (H.list . F.toChunks . E.toLazyByteString)
