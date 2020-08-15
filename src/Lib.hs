module Lib where

import Codec.Archive.Zip as Z
import Data.Maybe
import Data.List
import Text.Printf
import Data.Bifunctor

stripPrefix' :: Eq a => [a] -> [a] -> [a]
stripPrefix' p s = fromMaybe s $ stripPrefix p s  


printfn s = printf (s ++ "\n")


getCompressionMethod :: EntrySelector -> ZipArchive (Maybe CompressionMethod)
getCompressionMethod = fmap (fmap edCompression) . getEntryDesc

lookupR :: Eq k => k -> [(k, v)] -> Maybe (v, [(k, v)])
lookupR x [] = Nothing
lookupR x ((k, v):ys)
    | k == x = Just (v, ys)
    | otherwise = second ((k,v):) <$> lookupR x ys


