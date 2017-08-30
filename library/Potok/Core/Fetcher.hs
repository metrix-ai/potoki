module Potok.Core.Fetcher where

import Potok.Prelude
import qualified Data.ByteString as A
import qualified Data.Attoparsec.Types as I
import qualified Data.Attoparsec.ByteString as K
import qualified Data.Attoparsec.Text as L


newtype Fetcher element =
  {-|
  Church encoding of @IO (Maybe element)@.
  -}
  Fetcher (forall x. IO x -> (element -> IO x) -> IO x)

instance Functor Fetcher where
  fmap mapping (Fetcher fetcherFn) =
    Fetcher (\signalEnd signalElement -> fetcherFn signalEnd (signalElement . mapping))

instance Applicative Fetcher where
  pure x =
    Fetcher (\signalEnd signalElement -> signalElement x)
  (<*>) (Fetcher leftFn) (Fetcher rightFn) =
    Fetcher (\signalEnd signalElement -> leftFn signalEnd (\leftElement -> rightFn signalEnd (\rightElement -> signalElement (leftElement rightElement))))

instance Monad Fetcher where
  return =
    pure
  (>>=) (Fetcher leftFn) rightK =
    Fetcher
    (\signalEnd onRightElement ->
      leftFn signalEnd
      (\leftElement -> case rightK leftElement of
        Fetcher rightFn -> rightFn signalEnd onRightElement))

instance Alternative Fetcher where
  empty =
    Fetcher (\signalEnd signalElement -> signalEnd)
  (<|>) (Fetcher leftSignal) (Fetcher rightSignal) =
    Fetcher (\signalEnd signalElement -> leftSignal (rightSignal signalEnd signalElement) signalElement)

mapWithParseResult :: forall input parsed. (input -> I.IResult input parsed) -> Fetcher input -> IO (Fetcher (Either Text parsed))
mapWithParseResult inputToResult (Fetcher fetchInput) =
  do
    unconsumedStateRef <- newIORef Nothing
    return (Fetcher (fetchParsed unconsumedStateRef))
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
parseBytes :: K.Parser parsed -> Fetcher ByteString -> IO (Fetcher (Either Text parsed))
parseBytes parser =
  mapWithParseResult (K.parse parser)

{-|
Lift an Attoparsec Text parser.

Consumption is non-greedy and terminates when the parser is done.
-}
{-# INLINE parseText #-}
parseText :: L.Parser parsed -> Fetcher Text -> IO (Fetcher (Either Text parsed))
parseText parser =
  mapWithParseResult (L.parse parser)

duplicate :: Fetcher element -> IO (Fetcher element, Fetcher element)
duplicate (Fetcher fetchInput) =
  undefined

take :: Int -> Fetcher element -> IO (Fetcher element)
take amount (Fetcher fetchInput) =
  fetcher <$> newIORef amount
  where
    fetcher countRef =
      Fetcher $ \signalEnd signalElement -> do
        count <- readIORef countRef
        if count > 0
          then do
            writeIORef countRef (pred count)
            fetchInput signalEnd signalElement
          else signalEnd

sink :: (Fetcher input -> IO output) -> Fetcher input -> IO (Fetcher output)
sink sink (Fetcher signal) =
  fetcher <$> newIORef False
  where
    fetcher finishedRef =
      Fetcher $ \signalEnd signalOutput -> do
        finished <- readIORef finishedRef
        if finished
          then signalEnd
          else do
            output <-
              sink $ Fetcher $ \sinkSignalEnd sinkSignalInput ->
              signal (writeIORef finishedRef True >> sinkSignalEnd) sinkSignalInput
            signalOutput output

handleBytes :: Handle -> Int -> Fetcher (Either IOException ByteString)
handleBytes handle chunkSize =
  Fetcher $ \signalEnd signalElement ->
  do
    element <- try (A.hGetSome handle chunkSize)
    case element of
      Right "" -> signalEnd
      _ -> signalElement element

asMaybeIO :: Fetcher element -> IO (Maybe element)
asMaybeIO (Fetcher signal) =
  signal (pure Nothing) (pure . Just)

maybeIO :: IO (Maybe element) -> Fetcher element
maybeIO maybeIO =
  Fetcher (\signalEnd signalElement -> maybeIO >>= maybe signalEnd signalElement)

first :: (Fetcher input -> IO (Fetcher output)) -> (Fetcher (input, right) -> IO (Fetcher (output, right)))
first inputUpdate (Fetcher inputAndRightSignal) =
  outputAndRightFetcher <$> newIORef Nothing
  where
    outputAndRightFetcher rightStateRef =
      Fetcher $ \outputAndRightSignalEnd outputAndRightSignalElement ->
      do
        Fetcher outputSignal <-
          inputUpdate $ Fetcher $ \inputSignalEnd inputSignalElement ->
          inputAndRightSignal inputSignalEnd $ \(input, right) -> do
            writeIORef rightStateRef (Just right)
            inputSignalElement input
        outputSignal outputAndRightSignalEnd $ \output -> do
          rightState <- readIORef rightStateRef
          case rightState of
            Just right -> outputAndRightSignalElement (output, right)
            Nothing -> outputAndRightSignalEnd

mapFilter :: (input -> Maybe output) -> Fetcher input -> Fetcher output
mapFilter mapping (Fetcher fetch) =
  Fetcher $ \signalEnd signalOutput ->
  fix $ \loop ->
  fetch signalEnd $ \input ->
  case mapping input of
    Just output -> signalOutput output
    Nothing -> loop

list :: [input] -> IO (Fetcher input)
list list =
  fetcher <$> newIORef list
  where
    fetcher unsentListRef =
      Fetcher $ \signalEnd signalElement -> do
        list <- readIORef unsentListRef
        case list of
          head : tail -> do
            writeIORef unsentListRef tail
            signalElement head
          _ -> signalEnd
