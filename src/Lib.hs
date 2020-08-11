module Lib where

import Codec.Archive.Zip as Z 
import Data.Maybe
import Data.List
import Text.Printf

stripPrefix' :: Eq a => [a] -> [a] -> [a]
stripPrefix' p s = fromMaybe s $ stripPrefix p s  


printfn s = printf (s ++ "\n")


getCompressionMethod :: EntrySelector -> ZipArchive (Maybe CompressionMethod)
getCompressionMethod = fmap (fmap edCompression) . getEntryDesc


