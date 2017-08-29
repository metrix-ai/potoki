module Potok.AcquiredSink where

import Potok.Prelude
import qualified Potok.Fetcher as A


newtype AcquiredSink input =
  AcquiredSink (A.Fetcher input -> IO ())
