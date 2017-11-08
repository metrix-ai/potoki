module Potoki.Core.Transform where

import Potoki.Prelude
import qualified Potoki.Core.Fetch as A
import qualified Data.Attoparsec.ByteString as K
import qualified Data.Attoparsec.Text as L
import qualified Control.Concurrent.Chan.Unagi as B


newtype Transform input output =
  Transform (A.Fetch input -> IO (A.Fetch output))

instance Category Transform where
  id =
    Transform return
  (.) (Transform leftFetchIO) (Transform rightFetchIO) =
    Transform (leftFetchIO <=< rightFetchIO)

instance Profunctor Transform where
  dimap inputMapping outputMapping (Transform fetcherIO) =
    Transform (\inputFetch -> (fmap . fmap) outputMapping (fetcherIO (fmap inputMapping inputFetch)))

instance Strong Transform where
  first' (Transform io) =
    Transform (A.first io) 

{-|
The behaviour of this instance is that it stops on the first appearance of the opposite input.
I.e., if you focus on the left values, it'll apply the transform to all the left values up until
the first right value and will emit the result of the transform followed by the right value.
-}
instance Choice Transform where
  left' (Transform transform) =
    Transform $ \(A.Fetch fetchInputOrRight) -> do
      rightMaybeRef <- newIORef Nothing
      A.Fetch fetchOutput <-
        transform $ A.Fetch $ \stop emitInput ->
        fetchInputOrRight stop $ \case
          Left left -> emitInput left
          Right right -> do
            writeIORef rightMaybeRef (Just right)
            stop
      return $ A.Fetch $ \stop emitOutputOrRight -> do
        fetchOutput
          (do
            rightMaybe <- readIORef rightMaybeRef
            case rightMaybe of
              Just right -> do
                writeIORef rightMaybeRef Nothing
                emitOutputOrRight (Right right)
              Nothing -> stop)
          (\output -> emitOutputOrRight (Left output))

instance Arrow Transform where
  arr fn =
    Transform (pure . fmap fn)
  first =
    first'

instance ArrowChoice Transform where
  left =
    left'

{-|
Lift an Attoparsec ByteString parser.
-}
{-# INLINE parseBytes #-}
parseBytes :: K.Parser parsed -> Transform ByteString (Either Text parsed)
parseBytes parser =
  Transform (A.mapWithBytesParser parser)

{-|
Lift an Attoparsec Text parser.
-}
{-# INLINE parseText #-}
parseText :: L.Parser parsed -> Transform Text (Either Text parsed)
parseText parser =
  Transform (A.mapWithTextParser parser)

{-# INLINE take #-}
take :: Int -> Transform input input
take amount =
  Transform (A.take amount)

{-|
Same as 'arr'.
-}
{-# INLINE map #-}
map :: (input -> output) -> Transform input output
map mapping =
  arr mapping

{-# INLINE mapFilter #-}
mapFilter :: (input -> Maybe output) -> Transform input output
mapFilter mapping =
  Transform (pure . A.mapFilter mapping)

{-# INLINE just #-}
just :: Transform (Maybe input) input
just =
  Transform $ \(A.Fetch fetch) ->
  return $ A.Fetch $ \stop emit ->
  fix $ \loop ->
  fetch stop $ \case
    Just input -> emit input
    Nothing -> loop

{-# INLINE takeWhileIsJust #-}
takeWhileIsJust :: Transform (Maybe input) input
takeWhileIsJust =
  Transform (\(A.Fetch fetch) ->
    return (A.Fetch (\stop emit ->
      fetch stop (\case
        Just input -> emit input
        Nothing -> stop))))

{-# INLINE takeWhile #-}
takeWhile :: (input -> Bool) -> Transform input input
takeWhile predicate =
  Transform $ \(A.Fetch fetch) ->
  return $ A.Fetch $ \stop emit ->
  fetch stop $ \input ->
  if predicate input
    then emit input
    else stop

{-# INLINE consume #-}
consume :: (A.Fetch input -> IO output) -> Transform input output
consume consume =
  Transform $ \(A.Fetch fetch) -> do
    stoppedRef <- newIORef False
    return $ A.Fetch $ \stopOutput emitOutput -> do
      stopped <- readIORef stoppedRef
      if stopped
        then stopOutput
        else do
          emittedRef <- newIORef False
          output <- consume $ A.Fetch $ \stopInput emitInput ->
            fetch
              (do
                writeIORef stoppedRef True
                stopInput)
              (\input -> do
                writeIORef emittedRef True
                emitInput input)
          stopped <- readIORef stoppedRef
          if stopped
            then do
              emitted <- readIORef emittedRef
              if emitted
                then emitOutput output
                else stopOutput
            else emitOutput output

{-# INLINE bufferize #-}
bufferize :: Transform element element
bufferize =
  Transform $ \(A.Fetch fetch) -> do
    (inChan, outChan) <- B.newChan
    forkIO $ fix $ \ loop ->
      fetch
        (B.writeChan inChan Nothing)
        (\ element -> do
          B.writeChan inChan $! Just $! element
          loop)
    return $ A.Fetch $ \stop emit -> B.readChan outChan >>= maybe stop emit

{-|
Execute the IO action.
-}
{-# INLINE executeIO #-}
executeIO :: Transform (IO a) a
executeIO =
  Transform $ \(A.Fetch fetch) ->
  return $ A.Fetch $ \stop emit ->
  fetch stop (\ io -> io >>= emit)
