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
import qualified Data.ByteString.UTF8 as BU
import Types
import System.Process as P
import Data.Bifunctor
import System.IO


main :: IO ()
main = do
    args <- getArgs
    if null args then
        usage
    else do
        let fname = head args
        f <- (\x -> if x then withArchive else createArchive) <$> doesFileExist fname
        doesFileExist "~/.zipSHHistory" >>= (\x -> when (not x) $ void $ system "touch ~/.zipSHHistory")
        f fname (execStateT (repl fname) defaultState)
        return ()


repl :: String -> Repl ()
repl fname = do
    pwd <- statePath
    cmd <- liftIO $ runInputT defaultSettings {historyFile = Just "~/.zipSHHistory"} $ getCmd fname (pathToStr pwd)
    case cmd of
        "exit" -> return ()
        _ -> do
            when (not $ null $ words cmd) $ runCmd cmd
            repl fname

getCmd :: String -> String -> InputT IO String
getCmd zipName pwd = withInterrupt (fmap (fromMaybe "exit") $ getInputLine $ printf "[%s]:%s/$ " zipName pwd) `catch` (\Interrupt -> return "")

runCmd :: String -> Repl ()
runCmd cmd = do
    let symbols = words cmd
    case M.lookup (head symbols) cmds of
        Just x -> x $ tail symbols
        Nothing -> customCommand symbols

cmds :: M.Map String ([String] -> Repl ())
cmds = M.fromList [
        ("ls", \as -> case as of
            [] -> statePath >>= ls >>= (liftIO .  putStrLn . unwords)
            [x] -> statePath >>= (\p -> ls (p `joinPath` pathFromStr x)) >>= (liftIO .  putStrLn . unwords)
            _ -> tooManyArgs "ls"),
        ("pwd", const $ pwd >>= (liftIO .  putStrLn . pathToStr)),
        ("cd", \as -> case as of
            [] -> cd (Path []) >> return ()
            [x] -> statePath >>= \p -> cd (p `joinPath` pathFromStr x) >>= (\b -> when (not b) (liftIO $ printfn "cannot cd to '%s'" $ x))
            _ -> tooManyArgs "cd"),
        ("cat", \as -> case as of
            [] -> notEnoughArgs "cat"
            _ -> statePath >>= \p -> cat (map (joinPath p . pathFromStr) as) >>= (liftIO . putStrLn))
        --("undoAll", const $ lift $ (liftIO $ runInputT defaultSettings undoAllPrompt) >>= (\x -> if x then undoAll else liftIO (putStrLn "Aborting undo")))
    ]


-- | Currently not working because of calls to @commit@. May work in the future
undoAllPrompt :: InputT IO Bool
undoAllPrompt = do
    x <- fromMaybe "n" <$> getInputLine "Really undo EVERY action from the current session (y/n)? "
    return $ x`elem`["y","Y"]


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
                Just (Directory _) -> return $ printf "cat: %s: Is a directory" (pathToStr p)
                Just (File _) -> BU.toString <$> lift ((mkEntrySelector (pathToStr p)) >>= getEntry)
                Nothing -> return $ printf "cat: %s: No such file or directory" (pathToStr p)

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

--TODO: Error message if directory does not exist
ls :: Path -> Repl [String]
ls p = nub . map (head . getSegments . entryPath) . directEntries <$> relativeEntries p


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


