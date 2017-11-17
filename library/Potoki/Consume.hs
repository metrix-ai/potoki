module Potoki.Consume
(
  Consume,
  transform,
  count,
  sum,
  head,
  list,
  reverseList,
  concat,
  fold,
  foldInIO,
  writeBytesToFile,
  appendBytesToFile,
  deleteFiles,
  printBytes,
  printText,
  printString,
  parseBytes,
  parseText,
)
where

import Potoki.Prelude hiding (sum, head, fold, concat)
import Potoki.Core.Consume
import qualified Potoki.Core.Fetch as A
import qualified Potoki.Core.Produce as H
import qualified Potoki.Core.Transform as J
import qualified Potoki.Core.IO as L
import qualified Data.ByteString as C
import qualified Data.Attoparsec.ByteString as E
import qualified Data.Attoparsec.Text as F
import qualified Data.Attoparsec.Types as I
import qualified Data.Text.IO as K
import qualified Control.Foldl as D
import qualified System.Directory as G


{-# INLINABLE transform #-}
transform :: J.Transform input output -> Consume output sinkOutput -> Consume input sinkOutput
transform (J.Transform transform) (Consume sink) =
  Consume (transform >=> sink)

{-# INLINABLE head #-}
head :: Consume input (Maybe input)
head =
  Consume (\ (A.Fetch fetchIO) -> fetchIO Nothing Just)

{-|
A faster alternative to "list",
which however constructs the list in the reverse order.
-}
{-# INLINABLE reverseList #-}
reverseList :: Consume input [input]
reverseList =
  Consume $ \ (A.Fetch fetchIO) -> build fetchIO []
  where
    build fetchIO !acc =
      join (fetchIO (pure acc) (\ element -> build fetchIO (element : acc)))

{-# INLINABLE count #-}
count :: Consume input Int
count =
  Consume $ \ (A.Fetch fetchIO) -> build fetchIO 0
  where
    build fetchIO !acc =
      join (fetchIO (pure acc) (const (build fetchIO (succ acc))))

{-# INLINABLE concat #-}
concat :: Monoid monoid => Consume monoid monoid
concat =
  Consume $ \ (A.Fetch fetchIO) -> build fetchIO mempty
  where
    build fetchIO !acc =
      join (fetchIO (pure acc) (\ x -> build fetchIO (mappend acc x)))

{-# INLINABLE processInIO #-}
processInIO :: (element -> IO ()) -> Consume element ()
processInIO process =
  Consume (\ fetch -> L.fetchAndHandleAll fetch process)

{-# INLINABLE printBytes #-}
printBytes :: Consume ByteString ()
printBytes =
  processInIO C.putStr

{-# INLINABLE printText #-}
printText :: Consume Text ()
printText =
  processInIO K.putStr

{-# INLINABLE printString #-}
printString :: Consume String ()
printString =
  processInIO putStr

{-|
Overwrite a file.

* Exception-free
* Automatic resource management
-}
{-# INLINABLE writeBytesToFile #-}
writeBytesToFile :: FilePath -> Consume ByteString (Either IOException ())
writeBytesToFile path =
  Consume $ \ fetch ->
  try $ withFile path WriteMode $ \ handle ->
  L.fetchAndHandleAll fetch $ \ bytes -> 
  C.hPut handle bytes

{-|
Append to a file.

* Exception-free
* Automatic resource management
-}
{-# INLINABLE appendBytesToFile #-}
appendBytesToFile :: FilePath -> Consume ByteString (Either IOException ())
appendBytesToFile path =
  Consume $ \ fetch ->
  try $ withFile path AppendMode $ \ handle ->
  L.fetchAndHandleAll fetch $ \ bytes -> 
  C.hPut handle bytes

{-# INLINABLE deleteFiles #-}
deleteFiles :: Consume FilePath (Either IOException ())
deleteFiles =
  Consume $ \ fetch ->
  try $ L.fetchAndHandleAll fetch G.removeFile

{-# INLINABLE fold #-}
fold :: D.Fold input output -> Consume input output
fold (D.Fold step init finish) =
  Consume $ \ (A.Fetch fetch) -> build fetch init
  where
    build fetch !accumulator =
      join (fetch (pure (finish accumulator)) (\ !input -> build fetch (step accumulator input)))

{-# INLINABLE foldInIO #-}
foldInIO :: D.FoldM IO input output -> Consume input output
foldInIO (D.FoldM step init finish) =
  Consume $ \ (A.Fetch fetch) -> build fetch =<< init
  where
    build fetch !accumulator =
      join (fetch (finish accumulator) (\ !input -> step accumulator input >>= build fetch))

{-# INLINABLE runParseResult #-}
runParseResult :: (Monoid input, Eq input) => (input -> I.IResult input output) -> Consume input (Either Text output)
runParseResult inputToResult =
  Consume $ \ (A.Fetch fetchInput) ->
  let
    consume inputToResult =
      join (fetchInput nil just)
      where
        nil =
          just mempty
        just !input =
          case inputToResult input of
            I.Partial newInputToResult -> consume newInputToResult
            I.Done _ parsed -> return (Right parsed)
            I.Fail _ contexts message -> return (Left resultMessage)
              where
                resultMessage =
                  if null contexts
                    then fromString message
                    else fromString (showString (intercalate " > " contexts) (showString ": " message))
    in consume inputToResult

{-# INLINABLE parseBytes #-}
parseBytes :: E.Parser output -> Consume ByteString (Either Text output)
parseBytes =
  runParseResult . E.parse

{-# INLINABLE parseText #-}
parseText :: F.Parser output -> Consume Text (Either Text output)
parseText =
  runParseResult . F.parse
