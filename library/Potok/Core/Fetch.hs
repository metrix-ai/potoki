module Potok.Core.Fetch where

import Potok.Prelude
import qualified Data.ByteString as A
import qualified Deque as B
import qualified Data.Attoparsec.Types as I
import qualified Data.Attoparsec.ByteString as K
import qualified Data.Attoparsec.Text as L


newtype Fetch element =
  {-|
  Church encoding of @IO (Maybe element)@.
  -}
  Fetch (forall x. IO x -> (element -> IO x) -> IO x)

instance Functor Fetch where
  fmap mapping (Fetch fetcherFn) =
    Fetch (\stop emit -> fetcherFn stop (emit . mapping))

instance Applicative Fetch where
  pure x =
    Fetch (\stop emit -> emit x)
  (<*>) (Fetch leftFn) (Fetch rightFn) =
    Fetch (\stop emit -> leftFn stop (\leftElement -> rightFn stop (\rightElement -> emit (leftElement rightElement))))

instance Monad Fetch where
  return =
    pure
  (>>=) (Fetch leftFn) rightK =
    Fetch
    (\stop emit ->
      leftFn stop
      (\leftElement -> case rightK leftElement of
        Fetch rightFn -> rightFn stop emit))

instance Alternative Fetch where
  empty =
    Fetch (\stop emit -> stop)
  (<|>) (Fetch leftSignal) (Fetch rightSignal) =
    Fetch (\stop emit -> leftSignal (rightSignal stop emit) emit)

asMaybeIO :: Fetch element -> IO (Maybe element)
asMaybeIO (Fetch signal) =
  signal (pure Nothing) (pure . Just)

maybeIO :: IO (Maybe element) -> Fetch element
maybeIO maybeIO =
  Fetch (\stop emit -> maybeIO >>= maybe stop emit)

list :: IORef [input] -> Fetch input
list unsentListRef =
  Fetch $ \stop emit -> do
    list <- readIORef unsentListRef
    case list of
      head : tail -> do
        writeIORef unsentListRef tail
        emit head
      _ -> stop

mapWithParseResult :: forall input parsed. (Monoid input, Eq input) => (input -> I.IResult input parsed) -> Fetch input -> IO (Fetch (Either Text parsed))
mapWithParseResult inputToResult (Fetch fetchInput) =
  do
    unconsumedRef <- newIORef mempty
    finishedRef <- newIORef False
    return (Fetch (fetchParsed finishedRef unconsumedRef))
  where
    fetchParsed :: IORef Bool -> IORef input -> IO x -> (Either Text parsed -> IO x) -> IO x
    fetchParsed finishedRef unconsumedRef stop emit =
      do
        finished <- readIORef finishedRef
        if finished
          then stop
          else do
            unconsumed <- readIORef unconsumedRef
            if unconsumed == mempty
              then
                fetchInput
                  stop
                  (\input -> do
                    if input == mempty
                      then do
                        stop
                      else matchResult (inputToResult input))
              else do
                writeIORef unconsumedRef mempty
                matchResult (inputToResult unconsumed)
      where
        matchResult =
          \case
            I.Partial inputToResult ->
              consume inputToResult
            I.Done unconsumed parsed ->
              do
                writeIORef unconsumedRef unconsumed
                emit (Right parsed)
            I.Fail unconsumed contexts message ->
              do
                writeIORef unconsumedRef unconsumed
                emit (Left (fromString (intercalate " > " contexts <> ": " <> message)))
        consume inputToResult =
          fetchInput
            (do
              writeIORef finishedRef True
              matchResult (inputToResult mempty))
            (\input -> do
              when (input == mempty) (writeIORef finishedRef True)
              matchResult (inputToResult input))

{-|
Lift an Attoparsec ByteString parser.

Consumption is non-greedy and terminates when the parser is done.
-}
{-# INLINE parseBytes #-}
parseBytes :: K.Parser parsed -> Fetch ByteString -> IO (Fetch (Either Text parsed))
parseBytes parser =
  mapWithParseResult (K.parse parser)

{-|
Lift an Attoparsec Text parser.

Consumption is non-greedy and terminates when the parser is done.
-}
{-# INLINE parseText #-}
parseText :: L.Parser parsed -> Fetch Text -> IO (Fetch (Either Text parsed))
parseText parser =
  mapWithParseResult (L.parse parser)

duplicate :: Fetch element -> IO (Fetch element, Fetch element)
duplicate (Fetch fetchInput) =
  undefined

take :: Int -> Fetch element -> IO (Fetch element)
take amount (Fetch fetchInput) =
  fetcher <$> newIORef amount
  where
    fetcher countRef =
      Fetch $ \stop emit -> do
        count <- readIORef countRef
        if count > 0
          then do
            writeIORef countRef (pred count)
            fetchInput stop emit
          else stop

consume :: (Fetch input -> IO output) -> Fetch input -> Fetch output
consume consume (Fetch fetchInput) =
  Fetch $ \stop emit -> do
    peeked <- fetchInput (pure Nothing) (pure . Just)
    case peeked of
      Nothing -> stop
      Just peeked -> do
        firstRef <- newIORef (Just peeked)
        output <-
          consume $ Fetch $ \consumeStop consumeEmit -> do
            firstMaybe <- readIORef firstRef
            case firstMaybe of
              Just first -> do
                writeIORef firstRef Nothing
                consumeEmit first
              Nothing ->
                fetchInput consumeStop consumeEmit
        emit output

handleBytes :: Handle -> Int -> Fetch (Either IOException ByteString)
handleBytes handle chunkSize =
  Fetch $ \stop emit ->
  do
    element <- try (A.hGetSome handle chunkSize)
    case element of
      Right "" -> stop
      _ -> emit element

first :: (Fetch input -> IO (Fetch output)) -> (Fetch (input, right) -> IO (Fetch (output, right)))
first inputUpdate (Fetch fetchInputAndRight) =
  do
    rightStateRef <- newIORef mempty
    outputFetch <- inputUpdate (inputFetch rightStateRef)
    return (outputAndRightFetch rightStateRef outputFetch)
  where
    inputFetch rightStateRef =
      Fetch $ \stop emit ->
      fetchInputAndRight stop $ \(input, right) -> do
        modifyIORef rightStateRef (B.snoc right)
        emit input
    outputAndRightFetch rightStateRef (Fetch fetchOutput) =
      Fetch $ \stop emit ->
      fetchOutput stop $ \output -> do
        rightState <- readIORef rightStateRef
        case B.uncons rightState of
          Just (right, rightStateTail) -> do
            writeIORef rightStateRef rightStateTail
            emit (output, right)
          Nothing -> stop

left :: (Fetch input -> IO (Fetch output)) -> (Fetch (Either input right) -> IO (Fetch (Either output right)))
left inputUpdate (Fetch inputOrRightSignal) =
  do
    bufferRef <- newIORef mempty
    outputFetch <- inputUpdate (inputFetch bufferRef)
    return (outputOrRightFetch bufferRef outputFetch)
  where
    inputFetch bufferRef =
      Fetch $ \stop emit ->
      fix $ \loop ->
      inputOrRightSignal stop $ \case
        Left input -> emit input
        Right right -> modifyIORef bufferRef (B.snoc (Right right)) >> loop
    outputOrRightFetch bufferRef (Fetch fetchOutput) =
      Fetch $ \stop emit ->
        let
          outputStop =
            do
              buffer <- readIORef bufferRef
              case B.uncons buffer of
                Just (outputOrRight, tailRightState) -> do
                  writeIORef bufferRef tailRightState
                  emit outputOrRight
                Nothing ->
                  stop
          outputEmit output =
            do
              buffer <- readIORef bufferRef
              case B.uncons buffer of
                Just (outputOrRight, tailRightState) -> do
                  writeIORef bufferRef (B.snoc (Left output) tailRightState)
                  emit outputOrRight
                Nothing -> emit (Left output)
          in
            fetchOutput outputStop outputEmit

mapFilter :: (input -> Maybe output) -> Fetch input -> Fetch output
mapFilter mapping (Fetch fetch) =
  Fetch $ \stop emit ->
  fix $ \loop ->
  fetch stop $ \input ->
  case mapping input of
    Just output -> emit output
    Nothing -> loop
