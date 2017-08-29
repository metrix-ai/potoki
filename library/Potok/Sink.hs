module Potok.Sink where

import Potok.Prelude
import qualified Potok.Fetcher as A
import qualified Control.Concurrent.Async as B


newtype Sink input =
  {-|
  An action, which uses a provided fetcher to perform IO,
  while managing the resources behind the scenes.
  -}
  Sink (A.Fetcher input -> IO ())

instance Contravariant Sink where
  contramap mapping (Sink io) =
    Sink (io . fmap mapping)

instance Divisible Sink where
  divide division (Sink leftIO) (Sink rightIO) =
    Sink $ \fetcher ->
    do
      (leftFetcher, rightFetcher) <- A.duplicate (fmap division fetcher)
      B.concurrently_ (leftIO (fmap fst leftFetcher)) (rightIO (fmap snd rightFetcher))
  conquer =
    Sink (const (pure ()))
