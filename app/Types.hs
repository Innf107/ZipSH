{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Control.Monad.State
import Codec.Archive.Zip
import Data.Maybe
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Lib
import Data.Either as E
import Data.Bifunctor
import System.Console.Haskeline.MonadException

type Repl a = StateT ReplState ZipArchive a

instance MonadException (StateT ReplState ZipArchive) where
    controlIO f = StateT $ \s -> controlIO $ \(RunIO run) -> let
                    run' = RunIO (fmap (StateT . const) . run . flip runStateT s)
                    in fmap (flip runStateT s) $ f run'

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
pathFromStr "~" = Path $ [""]
pathFromStr ('~':ss) = pathFromStr (ss)
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




--                                 Flag Arg Counts
data Syntax = Syntax [SyntaxPart] [(String, Int)] ([Flag] -> [(String, Arg)] -> Repl ())

instance Show Syntax where
    show = showSyntax

data SyntaxPart = SRequired SyntaxArg
                | SOptional SyntaxArg
                deriving (Show, Eq)

data SyntaxArg = SLit String
               | SArg String
               | SFlags
               | SFlag String
               deriving (Show, Eq)

type Arg = String
data Flag = Flag String [Arg]
          deriving (Show, Eq)

data Command = Command {cmdInfo::String, cmdSyntax::[Syntax]}

--TODO: include function name
instance Show Command where
    show (Command info syntax) = "usage: " ++ showSyntaxs syntax ++ "\n\n" ++ info

ls' = Command {
        cmdInfo="lists the contents of a directory",
        cmdSyntax = [
            Syntax [SRequired (SLit "ls"), SOptional (SFlags), SOptional (SArg "directory")] [("r", 0)] (\_ _ -> return ())
        ]
    }

testC = Command {
        cmdInfo="test",
        cmdSyntax = [
            Syntax [SRequired (SLit "Test")] [] (\_ _ -> return ())
        ]
    }

command :: Command -> [String] -> Repl ()
command (cmd@(Command {cmdInfo, cmdSyntax})) args = head $ (catMaybes $ map (`trySyntax`args) (cmdSyntax)) ++ [help cmd]
    where
        help = liftIO . putStrLn . show

trySyntax :: Syntax -> [String] -> Maybe (Repl ())
trySyntax (Syntax parts flagCounts f) args = do
        argsFlags <- parseArgs flagCounts args
        (flags, args) <- tryParts parts argsFlags
        return $ f flags args

tryParts :: [SyntaxPart] -> [Either Arg Flag] -> Maybe ([Flag], [(String, Arg)])
tryParts [] [] = Just ([], [])
tryParts [] _ = Nothing
tryParts _ [] = Nothing
tryParts (p:ps) (a:as) = case p of
    SOptional x ->           undefined
    SRequired (SLit x) -> case a of
        Right _ -> Nothing
        Left y -> guard (x == y) >> tryParts ps as
    SRequired (SArg x) -> case a of
        Right _ -> Nothing
        Left y -> second ((x,y):) <$> tryParts ps as
    SRequired (SFlags) ->    undefined
    SRequired (SFlag f) ->   case a of
        Left _ -> Nothing
        Right flag@(Flag y _) -> guard (f==y) >> first (flag:) <$> tryParts ps as


parseArgs :: ([(String, Int)]) -> [String] -> Maybe [Either Arg Flag]
parseArgs _ [] = Just []
parseArgs validFlags (a:as) = case a of
                ('-':'-':a') -> do
                        (argCount, validFlags') <- lookupR a' validFlags
                        guard (length as >= argCount)
                        let (fas, as') = splitAt argCount as
                        flagArgs <- E.lefts <$> parseArgs [] fas
                        ((Right (Flag a' flagArgs)):) <$> parseArgs validFlags' as'
                ('-':a') -> parseArgs validFlags (map (("--"++) . pure) a' ++ as)
                a' -> ((Left a'):) <$> parseArgs validFlags as

showSyntaxs :: [Syntax] -> String
showSyntaxs ss = intercalate " | " (map showSyntax ss)

showSyntax :: Syntax -> String
showSyntax (Syntax parts _ _) = unwords (map showSPart parts)

showSPart :: SyntaxPart -> String
showSPart (SRequired (SLit x)) = x
showSPart (SRequired x) = "<" ++ showSArg x ++ ">"
showSPart (SOptional x) = "[" ++ showSArg x ++ "]"

showSArg :: SyntaxArg -> String
showSArg (SLit x) = x
showSArg (SArg x) = x
showSArg (SFlags) = "flags"
showSArg (SFlag x) = "--" ++ x

-- ls --block-size 3
-- ls --block-size 3
