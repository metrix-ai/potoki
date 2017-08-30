module Potok.IO where

import Potok.Prelude
import qualified Potok.Core.Produce as A
import qualified Potok.Core.Consume as B
import qualified Potok.Core.Transform as C
import qualified Potok.Core.Fetch as D


produceAndConsume :: A.Produce input -> B.Consume input output -> IO output
produceAndConsume (A.Produce produce) (B.Consume consume) =
  produce consume
