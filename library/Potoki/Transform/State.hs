module Potoki.Transform.State
where

import Potoki.Prelude
import Potoki.Core.Transform
import qualified Potoki.Fetch as A
import qualified Potoki.Core.Fetch as A
import qualified Data.ByteString as B
import qualified Control.Monad.Trans.State.Strict as O


{-|
Notice that you can control the emission of output of each step
by producing a list of outputs and then composing the transform with
the "list" transform.
-}
{-# INLINE runState #-}
runState :: (a -> O.State s b) -> s -> Transform a (s, b)
runState stateFn initialState =
  Transform $ \ (A.Fetch fetchIO) -> do
    stateRef <- newIORef initialState
    return $ A.Fetch $ \ nil just -> do
      let
        nilIO =
          return nil
        justIO input =
          do
            currentState <- readIORef stateRef
            case O.runState (stateFn input) currentState of
              (output, newState) -> do
                writeIORef stateRef newState
                return (just (newState, output))
        in join (fetchIO nilIO justIO)

{-# INLINE evalState #-}
evalState :: (a -> O.State s b) -> s -> Transform a b
evalState stateFn initialState =
  runState stateFn initialState >>> arr snd

{-# INLINE execState #-}
execState :: (a -> O.State s b) -> s -> Transform a s
execState stateFn initialState =
  runState stateFn initialState >>> arr fst
