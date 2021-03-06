module Main where

import Lib
import Codec.Archive.Zip as Z
import System.Environment
import System.Directory
import qualified Data.Map as M
import Control.Monad
import Control.Monad.IO.Class
import System.Console.Haskeline
import Data.Maybe
import Data.Monoid
import Data.Foldable as F
import Control.Monad.State
import Text.Printf
import Data.List
import Data.List.Split
import Data.List.Extra as E
import qualified Data.ByteString.UTF8 as BU
import Types
import System.Process as P
import Data.Bifunctor
import System.IO
import AnsiEscapeCodes as A


main :: IO ()
main = do
    args <- getArgs
    if null args then
        usage
    else do
        let fname = head args
        f <- (\x -> if x then withArchive else createArchive) <$> doesFileExist fname
        f fname (execStateT (repl fname) defaultState)
        return ()


repl :: String -> Repl ()
repl fname = do
    pwd <- statePath
    historyFile <- liftIO $ (++ "/.zipSHHistory") <$> getHomeDirectory
    cmd <- runInputT (setComplete completion defaultSettings {historyFile = Just historyFile}) $ getCmd fname (pathToStr pwd)
    case cmd of
        "exit" -> return ()
        _ -> do
            when (not $ null $ words cmd) $ runCmd cmd
            repl fname

completion :: CompletionFunc (StateT ReplState ZipArchive)
completion = completeWord Nothing " \t\n" f
    where
        f :: String -> Repl [Completion]
        f l = do
            p <- statePath
            let l' = takeWhileEnd (/='/') l
            let p' = p <> (pathFromStr (dropWhileEnd (/='/') l))
            es <- ls p'
            return $ map (simpleCompletion . (l++)) $ catMaybes $ map (stripPrefix l') es

-- p' = p <> pathFromStr "test"
-- test/te


getCmd :: String -> String -> InputT (StateT ReplState ZipArchive) String
getCmd zipName pwd = do
    u <- liftIO $ getEnv "USER"
    withInterrupt (fmap (fromMaybe "exit") $ getInputLine $ printf "\x1b[32;1m%s@[%s]\x1b[0m:\x001b[34;1m~%s\x1b[0m$ " u zipName (if null pwd then "" else "/" ++ pwd)) `catch` (\Interrupt -> return "")

runCmd :: String -> Repl ()
runCmd cmd = do
    let symbols = words cmd
    case M.lookup (head symbols) cmds of
        Just x -> x $ tail symbols
        Nothing -> customCommand symbols

cmds :: M.Map String ([String] -> Repl ())
cmds = M.fromList [
        ("ls", command ls'),
        --("ls", \as -> case as of
        --    [] -> statePath >>= ls >>= (liftIO .  putStrLn . unwords)
        --    [x] -> statePath >>= (\p -> ls (p `joinPath` pathFromStr x)) >>= (liftIO .  putStrLn . unwords)
        --    _ -> tooManyArgs "ls"),
        ("pwd", const $ pwd >>= (liftIO .  putStrLn . pathToStr)),
        ("cd", \as -> case as of
            [] -> cd (Path []) >> return ()
            [x] -> statePath >>= \p -> cd (p `joinPath` pathFromStr x) >>= (\b -> when (not b) (liftIO $ printfn "cannot cd to '%s'" $ x))
            _ -> tooManyArgs "cd"),
        ("cat", \as -> case as of
            [] -> notEnoughArgs "cat"
            _ -> statePath >>= \p -> cat (map (joinPath p . pathFromStr) as) >>= (liftIO . putStr))
    ]

ls' = Command {
        cmdInfo="lists the contents of a directory",
        cmdSyntax = [
            Syntax [SOptional (SFlags), SOptional (SArg "directory")] [("a",0),("A",0)] (\fs as -> do
                let dir = lookup "directory" as
                sp <- statePath
                let p = sp <> pathFromStr (fromMaybe "" dir)
                res <- nub . map (head . getSegments . entryPath) . directEntries <$> relativeEntries p
                case res of
                    [] -> liftIO $ printfn "ls: cannot access '%s': No such file or directory" (pathToStr p) 
                    _ -> liftIO $ putStrLn $ unwords res
                )
        ]
    }

--TODO: Error message if directory does not exist
ls :: Path -> Repl [String]
ls p = nub . map (head . getSegments . entryPath) . directEntries <$> relativeEntries p


customCommand :: [String] -> Repl ()
customCommand cmd = do
    res <- externalFiles $ tail cmd
    case res of
        Just (external, processed, dir) -> (liftIO $ void (system (unwords (processed))) `catch` processHandler) >> cleanupFiles dir external
        Nothing -> return ()
        where
            processHandler :: IOException -> IO ()
            processHandler e = print e

            externalFiles :: [String] -> Repl (Maybe ([(String, CompressionMethod)], [String], String))
            externalFiles fs = do
                dir <-  liftIO $ (++"/zipSH/") <$> getTemporaryDirectory
                liftIO $ createDirectoryIfMissing True dir

                let external = map (stripPrefix' "§") $ filter ("§"`isPrefixOf`) cmd
                let processed = map (\x -> case stripPrefix "§" x of
                                        Nothing -> x
                                        Just x' -> dir ++ (last $ getSegments $ pathFromStr x')) cmd

                p <- statePath
                pSel <- mapM (pathToSelector . (p<>) . pathFromStr) $ external
                inValid <- lift $ filterM (\x -> not <$> doesEntryExist x) pSel

                case inValid of
                    [] -> do
                        cmethods <- mapM (saveToExternal dir) external
                        return $ Just (zip external cmethods, processed, dir)
                    _ -> liftIO $ printfn "The following files do not exist in this archive: %s" (show inValid) >> return Nothing

            saveToExternal :: FilePath -> String -> Repl CompressionMethod
            saveToExternal dir s = do
                p <- statePath
                let path = p <> (pathFromStr s)
                pathSelector <- pathToSelector $ path
                cmethod <- lift $ fromMaybe (error "Invalid entry") <$> getCompressionMethod pathSelector
                lift $ saveEntry pathSelector (dir ++ last (getSegments path))
                return cmethod

            cleanupFiles :: String -> [(String, CompressionMethod)] -> Repl ()
            cleanupFiles dir external = do
                p <- statePath
                selectors <- mapM pathToSelector $ ((p <>) . pathFromStr . fst) <$> external
                let paths = first ((dir++) . last . getSegments. pathFromStr) <$> external
                lift $ mapM_ (\((p, cmethod), sel) -> loadEntry cmethod sel p) (zip paths selectors)
                lift commit
                liftIO $ mapM_ (removeFile . fst) paths


cat :: [Path] -> Repl String
cat ps = concat <$> mapM (cat') ps
    where
        cat' :: Path -> Repl String
        cat' p = do
            e <- getEntryAt p
            case e of
                Just (Directory _) -> return $ printf "cat: %s: Is a directory\n" (pathToStr p)
                Just (File _) -> BU.toString <$> lift ((mkEntrySelector (pathToStr p)) >>= getEntry)
                Nothing -> return $ printf "cat: %s: No such file or directory\n" (pathToStr p)

cd :: Path -> Repl Bool
cd p = do
    es <- nub . concatMap entriesFromPath <$> files
    if p`elem`([Path []] ++ (map entryPath $ getDirectories es)) then do
        updatePath (const p)
        return True
    else
        return False


pwd :: Repl Path
pwd = statePath

usage :: IO ()
usage = putStrLn "zipSH <filename>"

notEnoughArgs :: String -> Repl ()
notEnoughArgs = liftIO . printfn "Not enough args in a call to '%s'"

tooManyArgs :: String -> Repl ()
tooManyArgs = liftIO . printfn "Too many args in a call to '%s'"


statePath :: Repl Path
statePath = path <$> get

updatePath :: (Path -> Path) -> Repl ()
updatePath f = do
    s <- get
    let s' = s {path=f $ path s}
    put s'


files :: Repl [Path]
files = map (pathFromStr . unEntrySelector) . M.keys <$> lift getEntries

relativeEntries :: Path -> Repl [Entry]
relativeEntries p = nub . concatMap entriesFromPath . filter validPath . map (relativePath p) <$> files


getEntryAt :: Path -> Repl (Maybe Entry)
getEntryAt p = do
    es <- nub . concatMap entriesFromPath <$> files
    return $ if (File p)`elem`es then
        Just (File p)
    else if (Directory p`elem`es) then
        Just (Directory p)
    else Nothing


pathToSelector :: Path -> Repl EntrySelector
pathToSelector = mkEntrySelector . pathToStr


