module Potok.Core.Sink where

import Potok.Prelude
import qualified Potok.Core.Fetcher as A
import qualified Potok.Core.Stream as C
import qualified Control.Concurrent.Async as B


{-|
The primary motivation for providing the @output@ type is the encoding of failures.
-}
newtype Sink input output =
  {-|
  An action, which uses a provided fetcher to perform IO,
  while managing the resources behind the scenes.
  -}
  Sink (A.Fetcher input -> IO output)


{-# INLINE stream #-}
stream :: C.Stream streamInput streamOutput -> Sink streamOutput output -> Sink streamInput output
stream (C.Stream streamIO) (Sink sinkIO) =
  Sink (streamIO >=> sinkIO)

{-# INLINE head #-}
head :: Sink input (Maybe input)
head =
  Sink (\(A.Fetcher send) -> send (pure Nothing) (pure . Just))

{-# INLINABLE list #-}
list :: Sink input [input]
list =
  Sink $ \(A.Fetcher send) -> build send id
  where
    build send acc =
      send (pure (acc [])) (\element -> build send ((:) element . acc))

{-|
A faster alternative to "list",
which however produces the list in the reverse order.
-}
{-# INLINABLE reverseList #-}
reverseList :: Sink input [input]
reverseList =
  Sink $ \(A.Fetcher send) -> build send []
  where
    build send acc =
      send (pure acc) (\element -> build send (element : acc))
