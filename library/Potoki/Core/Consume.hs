module Potoki.Core.Consume where

import Potoki.Prelude
import qualified Potoki.Core.Fetch as A
import qualified Control.Concurrent.Async as B
import qualified Data.ByteString as C
import qualified Control.Foldl as D
import qualified Data.Attoparsec.ByteString as E
import qualified Data.Attoparsec.Text as F
import qualified System.Directory as G


{-|
The primary motivation for providing the @output@ type is the encoding of failures.
-}
newtype Consume input output =
  {-|
  An action, which uses a provided fetcher to perform IO,
  while managing the resources behind the scenes.
  -}
  Consume (A.Fetch input -> IO output)

instance Profunctor Consume where
  {-# INLINE dimap #-}
  dimap inputMapping outputMapping (Consume consume) =
    Consume (\ fetch -> fmap outputMapping (consume (fmap inputMapping fetch)))

instance Functor (Consume input) where
  fmap = rmap

instance Applicative (Consume input) where
  pure x =
    Consume (const (pure x))
  (<*>) (Consume leftConsume) (Consume rightConsume) =
    Consume (\ fetch -> leftConsume fetch <*> rightConsume fetch)

{-# INLINE head #-}
head :: Consume input (Maybe input)
head =
  Consume (\ (A.Fetch send) -> send (pure Nothing) (pure . Just))

{-# INLINE list #-}
list :: Consume input [input]
list =
  Consume $ \ (A.Fetch send) -> build send id
  where
    build send !acc =
      send (pure (acc [])) (\ element -> build send (acc . (:) element))

{-|
A faster alternative to "list",
which however produces the list in the reverse order.
-}
{-# INLINE reverseList #-}
reverseList :: Consume input [input]
reverseList =
  Consume $ \ (A.Fetch send) -> build send []
  where
    build send !acc =
      send (pure acc) (\ element -> build send (element : acc))

{-# INLINE sum #-}
sum :: Num num => Consume num num
sum =
  Consume $ \ (A.Fetch send) -> build send 0
  where
    build send !acc =
      send (pure acc) (\ x -> build send (x + acc))

{-# INLINE count #-}
count :: Consume input Int
count =
  Consume $ \ (A.Fetch send) -> build send 0
  where
    build send !acc =
      send (pure acc) (const (build send (succ acc)))

{-# INLINE concat #-}
concat :: Monoid monoid => Consume monoid monoid
concat =
  Consume $ \ (A.Fetch send) -> build send mempty
  where
    build send !acc =
      send (pure acc) (\ x -> build send (mappend acc x))

{-# INLINE print #-}
print :: Show input => Consume input ()
print =
  Consume (\ fetch -> A.consume fetch Potoki.Prelude.print)

{-|
Overwrite a file.

* Exception-free
* Automatic resource management
-}
writeBytesToFile :: FilePath -> Consume ByteString (Maybe IOException)
writeBytesToFile path =
  Consume $ \ fetch -> do
    exceptionOrUnit <- 
      try $ withFile path WriteMode $ \ handle -> 
      A.consume fetch $ \ bytes -> 
      C.hPut handle bytes
    case exceptionOrUnit of
      Left exception -> return (Just exception)
      Right () -> return Nothing

{-|
Append to a file.

* Exception-free
* Automatic resource management
-}
appendBytesToFile :: FilePath -> Consume ByteString (Maybe IOException)
appendBytesToFile path =
  Consume $ \ fetch -> do
    exceptionOrUnit <- 
      try $ withFile path AppendMode $ \ handle -> 
      A.consume fetch $ \ bytes -> 
      C.hPut handle bytes
    case exceptionOrUnit of
      Left exception -> return (Just exception)
      Right () -> return Nothing

deleteFiles :: Consume FilePath (Maybe IOException)
deleteFiles =
  Consume $ \ fetch -> do
    exceptionOrUnit <- 
      try $ A.consume fetch G.removeFile
    case exceptionOrUnit of
      Left exception -> return (Just exception)
      Right () -> return Nothing

fold :: D.Fold input output -> Consume input output
fold (D.Fold step init finish) =
  Consume $ \ (A.Fetch fetch) -> build fetch init
  where
    build fetch !accumulator =
      fetch (pure (finish accumulator)) (\ !input -> build fetch (step accumulator input))

foldInIO :: D.FoldM IO input output -> Consume input output
foldInIO (D.FoldM step init finish) =
  Consume $ \ (A.Fetch fetch) -> build fetch =<< init
  where
    build fetch !accumulator =
      fetch (finish accumulator) (\ !input -> step accumulator input >>= build fetch)

parseBytes :: E.Parser output -> Consume ByteString (Either Text output)
parseBytes parser =
  Consume $ A.consumeWithBytesParser parser

parseText :: F.Parser output -> Consume Text (Either Text output)
parseText parser =
  Consume $ A.consumeWithTextParser parser
