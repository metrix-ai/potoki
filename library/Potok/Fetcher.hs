module Potok.Fetcher where

import Potok.Prelude
import qualified Data.Attoparsec.Types as I
import qualified Data.Attoparsec.ByteString as K
import qualified Data.Attoparsec.Text as L


newtype Fetcher element =
  Fetcher (forall x. IO x -> (element -> IO x) -> IO x)

instance Functor Fetcher where
  fmap mapping (Fetcher sourceFn) =
    Fetcher (\onEnd onElement -> sourceFn onEnd (onElement . mapping))

instance Applicative Fetcher where
  pure x =
    Fetcher (\onEnd onElement -> onElement x)
  (<*>) (Fetcher leftFn) (Fetcher rightFn) =
    Fetcher (\onEnd onElement -> leftFn onEnd (\leftElement -> rightFn onEnd (\rightElement -> onElement (leftElement rightElement))))

instance Monad Fetcher where
  return =
    pure
  (>>=) (Fetcher leftFn) rightK =
    Fetcher
    (\onEnd onRightElement ->
      leftFn onEnd
      (\leftElement -> case rightK leftElement of
        Fetcher rightFn -> rightFn onEnd onRightElement))

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
