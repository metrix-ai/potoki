module Potok.Core.Fetch where

import Potok.Prelude
import qualified Data.ByteString as A
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
    Fetch (\signalEnd signalElement -> fetcherFn signalEnd (signalElement . mapping))

instance Applicative Fetch where
  pure x =
    Fetch (\signalEnd signalElement -> signalElement x)
  (<*>) (Fetch leftFn) (Fetch rightFn) =
    Fetch (\signalEnd signalElement -> leftFn signalEnd (\leftElement -> rightFn signalEnd (\rightElement -> signalElement (leftElement rightElement))))

instance Monad Fetch where
  return =
    pure
  (>>=) (Fetch leftFn) rightK =
    Fetch
    (\signalEnd onRightElement ->
      leftFn signalEnd
      (\leftElement -> case rightK leftElement of
        Fetch rightFn -> rightFn signalEnd onRightElement))

instance Alternative Fetch where
  empty =
    Fetch (\signalEnd signalElement -> signalEnd)
  (<|>) (Fetch leftSignal) (Fetch rightSignal) =
    Fetch (\signalEnd signalElement -> leftSignal (rightSignal signalEnd signalElement) signalElement)

asMaybeIO :: Fetch element -> IO (Maybe element)
asMaybeIO (Fetch signal) =
  signal (pure Nothing) (pure . Just)

maybeIO :: IO (Maybe element) -> Fetch element
maybeIO maybeIO =
  Fetch (\signalEnd signalElement -> maybeIO >>= maybe signalEnd signalElement)

list :: IORef [input] -> Fetch input
list unsentListRef =
  Fetch $ \signalEnd signalElement -> do
    list <- readIORef unsentListRef
    case list of
      head : tail -> do
        writeIORef unsentListRef tail
        signalElement head
      _ -> signalEnd

mapWithParseResult :: forall input parsed. (input -> I.IResult input parsed) -> Fetch input -> IO (Fetch (Either Text parsed))
mapWithParseResult inputToResult (Fetch fetchInput) =
  do
    unconsumedStateRef <- newIORef Nothing
    return (Fetch (fetchParsed unconsumedStateRef))
  where
    fetchParsed :: IORef (Maybe input) -> IO x -> (Either Text parsed -> IO x) -> IO x
    fetchParsed unconsumedStateRef onParsedEnd onParsedElement =
      do
        unconsumedState <- readIORef unconsumedStateRef
        case unconsumedState of
          Just unconsumed -> matchResult (inputToResult unconsumed)
          Nothing -> consume inputToResult
      where
        matchResult =
          \case
            I.Partial inputToResult ->
              consume inputToResult
            I.Done unconsumed parsed ->
              do
                writeIORef unconsumedStateRef (Just unconsumed)
                onParsedElement (Right parsed)
            I.Fail unconsumed contexts message ->
              do
                writeIORef unconsumedStateRef (Just unconsumed)
                onParsedElement (Left (fromString (intercalate " > " contexts <> ": " <> message)))
        consume inputToResult =
          fetchInput onParsedEnd (matchResult . inputToResult)

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
      Fetch $ \signalEnd signalElement -> do
        count <- readIORef countRef
        if count > 0
          then do
            writeIORef countRef (pred count)
            fetchInput signalEnd signalElement
          else signalEnd

consume :: (Fetch input -> IO output) -> Fetch input -> IO (Fetch output)
consume consume (Fetch signal) =
  fetcher <$> newIORef False
  where
    fetcher finishedRef =
      Fetch $ \signalEnd signalOutput -> do
        finished <- readIORef finishedRef
        if finished
          then signalEnd
          else do
            output <-
              consume $ Fetch $ \consumeSignalEnd consumeSignalInput ->
              signal (writeIORef finishedRef True >> consumeSignalEnd) consumeSignalInput
            signalOutput output

handleBytes :: Handle -> Int -> Fetch (Either IOException ByteString)
handleBytes handle chunkSize =
  Fetch $ \signalEnd signalElement ->
  do
    element <- try (A.hGetSome handle chunkSize)
    case element of
      Right "" -> signalEnd
      _ -> signalElement element

first :: (Fetch input -> IO (Fetch output)) -> (Fetch (input, right) -> IO (Fetch (output, right)))
first inputUpdate (Fetch inputAndRightSignal) =
  outputAndRightFetch <$> newIORef Nothing
  where
    outputAndRightFetch rightStateRef =
      Fetch $ \outputAndRightSignalEnd outputAndRightSignalElement ->
      do
        Fetch outputSignal <-
          inputUpdate $ Fetch $ \inputSignalEnd inputSignalElement ->
          inputAndRightSignal inputSignalEnd $ \(input, right) -> do
            writeIORef rightStateRef (Just right)
            inputSignalElement input
        outputSignal outputAndRightSignalEnd $ \output -> do
          rightState <- readIORef rightStateRef
          case rightState of
            Just right -> outputAndRightSignalElement (output, right)
            Nothing -> outputAndRightSignalEnd

mapFilter :: (input -> Maybe output) -> Fetch input -> Fetch output
mapFilter mapping (Fetch fetch) =
  Fetch $ \signalEnd signalOutput ->
  fix $ \loop ->
  fetch signalEnd $ \input ->
  case mapping input of
    Just output -> signalOutput output
    Nothing -> loop
