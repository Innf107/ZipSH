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

-- | gets a value in an association list and removes it.
lookupR :: (Eq a) => a -> [(a, b)] -> Maybe (b, [(a, b)])
lookupR x [] = Nothing
lookupR x ((k,v):ys)
    | x == k = Just (v, ys)
    | otherwise = second ((k,v):) <$> lookupR x ys
