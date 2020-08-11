module Types where

import Control.Monad.State
import Codec.Archive.Zip
import Data.Maybe
import Data.List
import Data.List.Split


type Repl a = StateT ReplState ZipArchive a

data ReplState = ReplState {path::Path}
defaultState = ReplState (Path [])

newtype Path = Path {getSegments::[String]} deriving (Show, Eq)

instance Semigroup Path where
    (<>) = joinPath

data Entry = File Path
           | Directory Path deriving (Show, Eq)

isFile (File _) = True
isFile _ = False

isDirectory (Directory _) = True
isDirectory _ = False

getFiles = map (File . entryPath) . filter isFile
getDirectories = map (Directory . entryPath) . filter isDirectory


entryPath :: Entry -> Path
entryPath (File p) = p
entryPath (Directory p) = p

mapEntry :: (Path -> Path) -> Entry -> Entry
mapEntry f (File p)      = File $ f p
mapEntry f (Directory p) = Directory $ f p

mapPath :: ([String] -> [String]) -> Path -> Path
mapPath f (Path segs) = Path (f segs)


validPath :: Path -> Bool
validPath (Path segs) = not (null segs)

minimizePath :: Path -> Path
minimizePath (Path segs) = foldl joinPath (Path []) (map (Path . pure) segs)

pathFromStr :: String -> Path
pathFromStr "/" = Path $ [""]
pathFromStr s = Path $ dropWhileEnd (=="") $ splitOn "/" s

relativePath :: Path -> Path -> Path
relativePath (Path rel) (Path pa) = Path $ fromMaybe [] $ stripPrefix rel pa


directEntries :: [Entry] -> [Entry]
directEntries = filter ((==1) . length . getSegments . entryPath)

joinPath :: Path -> Path -> Path
joinPath (Path p1) (Path ("":ps)) = Path ps
joinPath (Path p1) (Path p2) = Path $ joinPath' p1 p2
    where
        joinPath' p1 [] = p1
        joinPath' p1 (x:xs) = case x of
            "." -> joinPath' p1 xs
            ".." -> joinPath' (take (length p1 - 1) p1) xs
            _ -> joinPath' (p1 ++ [x]) xs

pathToStr :: Path -> String
pathToStr (Path segs) = intercalate "/" segs

entriesFromPath :: Path -> [Entry]
entriesFromPath (Path [])     = []
entriesFromPath (Path [x])    = [File (Path [x])]
entriesFromPath (Path (x:xs)) = Directory (Path [x]) : (map (mapEntry (mapPath (x:))) $ entriesFromPath (Path xs))
