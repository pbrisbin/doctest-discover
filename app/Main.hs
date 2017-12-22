module Main where

import Config
import Control.Exception (IOException, catch)
import Control.Monad
import Runner
import System.Directory
import System.Environment
import System.FilePath

main :: IO ()
main = do
    (_ : _ : dst : args) <- getArgs
    let configFileContents = case args of
                              (configFile : _) -> readFile configFile
                              _                -> return ""
    customConfiguration <- config <$> configFileContents
    let sources = case customConfiguration of
                    Just (Config _ (Just sfs)) -> sfs
                    _                          -> ["app", "src"]
    files <- mapM getAbsDirectoryContents sources
    let testDriverFileContents = driver (concat files) customConfiguration
    writeFile dst testDriverFileContents

-- | Recursively get absolute directory contents
--
-- >>> :m +Data.List
-- >>> prefix <- getCurrentDirectory
-- >>> map (stripPrefix prefix) <$> getAbsDirectoryContents "test/example"
-- [Just "/test/example/Foo.hs",Just "/test/example/Foo/Bar.hs"]
--
-- >>> getAbsDirectoryContents "/does/not/exist"
-- []
--
getAbsDirectoryContents :: FilePath -> IO [FilePath]
getAbsDirectoryContents dir = do
    paths <- getDirectoryContents dir `catch` ignoreErrorWithEmpty
    paths' <- forM (filter (`notElem` [".", ".."]) paths) $ \path -> do
        canonicalized <- canonicalizePath $ dir </> path
        isDir <- doesDirectoryExist canonicalized
        if isDir
            then getAbsDirectoryContents canonicalized
            else return [canonicalized]
    return $ concat paths'
  where
    ignoreErrorWithEmpty :: Monad m => IOException -> m [a]
    ignoreErrorWithEmpty _ = return []
