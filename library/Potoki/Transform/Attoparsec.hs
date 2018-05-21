module Potoki.Transform.Attoparsec
where

import Potoki.Prelude hiding (take, takeWhile, filter, drop)
import Potoki.Core.Transform
import Potoki.Transform.Basic
import qualified Potoki.Fetch as A
import qualified Potoki.Core.Fetch as A
import qualified Data.Attoparsec.ByteString as K
import qualified Data.Attoparsec.Text as L
import qualified Data.Attoparsec.Types as M
import qualified Acquire.Acquire as N


{-# INLINE mapWithParseResult #-}
mapWithParseResult :: forall input parsed. (Monoid input, Eq input) => (input -> M.IResult input parsed) -> Transform input (Either Text parsed)
mapWithParseResult inputToResult =
  Transform $ N.Acquire $ do
    unconsumedRef <- newIORef mempty
    finishedRef <- newIORef False
    return (\inputFetch -> A.Fetch (fetchParsed inputFetch finishedRef unconsumedRef), return ())
  where
    fetchParsed :: A.Fetch input -> IORef Bool -> IORef input -> forall x. x -> (Either Text parsed -> x) -> IO x
    fetchParsed (A.Fetch inputFetchIO) finishedRef unconsumedRef nil just =
      do
        finished <- readIORef finishedRef
        if finished
          then return nil
          else do
            unconsumed <- readIORef unconsumedRef
            if unconsumed == mempty
              then
                join $ inputFetchIO
                  (return nil)
                  (\input -> do
                    if input == mempty
                      then return nil
                      else matchResult (inputToResult input))
              else do
                writeIORef unconsumedRef mempty
                matchResult (inputToResult unconsumed)
      where
        matchResult =
          \case
            M.Partial inputToResult ->
              consume inputToResult
            M.Done unconsumed parsed ->
              do
                writeIORef unconsumedRef unconsumed
                return (just (Right parsed))
            M.Fail unconsumed contexts message ->
              do
                writeIORef unconsumedRef unconsumed
                writeIORef finishedRef True
                return (just (Left resultMessage))
              where
                resultMessage =
                  if null contexts
                    then fromString message
                    else fromString (showString (intercalate " > " contexts) (showString ": " message))
        consume inputToResult =
          join $ inputFetchIO
            (do
              writeIORef finishedRef True
              matchResult (inputToResult mempty))
            (\input -> do
              when (input == mempty) (writeIORef finishedRef True)
              matchResult (inputToResult input))

{-|
Lift an Attoparsec ByteString parser.
-}
{-# INLINE parseBytes #-}
parseBytes :: K.Parser parsed -> Transform ByteString (Either Text parsed)
parseBytes parser =
  mapWithParseResult (K.parse parser)

{-|
Lift an Attoparsec Text parser.
-}
{-# INLINE parseText #-}
parseText :: L.Parser parsed -> Transform Text (Either Text parsed)
parseText parser =
  mapWithParseResult (L.parse parser)
