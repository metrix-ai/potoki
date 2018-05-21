module Potoki.Transform.ByteString
where

import Potoki.Prelude hiding (filter)
import Potoki.Core.Transform
import Potoki.Transform.Basic
import Potoki.Transform.State
import qualified Potoki.Fetch as A
import qualified Potoki.Core.Fetch as A
import qualified Potoki.Core.Produce as H
import qualified Ptr.Poking as C
import qualified Ptr.ByteString as D
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as E
import qualified Data.ByteString.Lazy as F
import qualified Control.Monad.Trans.State.Strict as O
import qualified Acquire.Acquire as M


{-# INLINE builderChunks #-}
builderChunks :: Transform E.Builder ByteString
builderChunks =
  produce (H.list . F.toChunks . E.toLazyByteString)

{-|
Convert freeform bytestring chunks into chunks,
which are strictly separated by newline no matter how long they may be.
-}
extractLines :: Transform ByteString ByteString
extractLines =
  lineList >>> filter (not . null) >>> list
  -- lineList >>> list
  where
    lineList =
      Transform $  M.Acquire $ do
        stateRef <- newIORef Nothing
        return $ (, return ()) $  \ (A.Fetch fetchIO) -> A.Fetch $ \ nil just -> join $ fetchIO
          (do
            state <- readIORef stateRef
            case state of
              Just poking -> do
                writeIORef stateRef Nothing
                return (just [D.poking poking])
              Nothing -> return nil)
          (\ chunk ->
            case B.split 10 chunk of
              firstInput : tail -> do
                state <- readIORef stateRef
                let
                  newPoking =
                    fold state <> C.bytes firstInput
                  in case unsnoc tail of
                    Just (init, last) ->
                      do
                        writeIORef stateRef (Just (C.bytes last))
                        return (just (D.poking newPoking : init))
                    Nothing ->
                      do
                        writeIORef stateRef (Just newPoking)
                        return (just [])
              _ -> return (just []))
