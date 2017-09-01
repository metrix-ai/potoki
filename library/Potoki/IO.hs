module Potoki.IO where

import Potoki.Prelude
import qualified Potoki.Core.Produce as A
import qualified Potoki.Core.Consume as B
import qualified Potoki.Core.Transform as C
import qualified Potoki.Core.Fetch as D


produceAndConsume :: A.Produce input -> B.Consume input output -> IO output
produceAndConsume (A.Produce produce) (B.Consume consume) =
  produce consume
