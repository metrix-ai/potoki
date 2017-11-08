module Potoki.Core.IO where

import Potoki.Prelude
import qualified Data.ByteString as C
import qualified System.Directory as G


appendBytesToFile :: FilePath -> ByteString -> IO (Either IOException ())
appendBytesToFile path bytes =
  try $ 
  withFile path AppendMode $ \ handle -> 
  C.hPut handle bytes

deleteFile :: FilePath -> IO (Either IOException ())
deleteFile path =
  try $
  G.removeFile path
