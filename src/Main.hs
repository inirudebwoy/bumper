#! /usr/bin/env runhaskell
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Main where

import Bumper
import System.Environment
import System.Console.CmdArgs
import Control.Monad (when)

data Bumper = Bumper
    {current_version :: String,
     suffix :: String,
     build :: String,
     part :: String,
     files :: [FilePath]
    } deriving (Show, Data, Typeable)


bumper :: Bumper
bumper = Bumper
         {current_version = def &= help "The current version of the software package before bumping." &= typ "VERSION",
          suffix = def &= help "Suffix to be added, e.g., alpha, rc-2" &= typ "SUFFIX",
          build = def &= help "Build number to be added, e.g., b42, f7a8051" &= typ "BUILD",
          part = def &= argPos 0 &= typ "PART",
          files = def &= args &= typ "FILES/DIRS"
         }

getOpts :: IO Bumper
getOpts = cmdArgs $ bumper
          &= help ("Version bumping software.")
          &= summary "bumper 0.0.1, Michał Klich"

main :: IO ()
main = do
  callArgs <- getArgs
  opts <- (if null callArgs then withArgs ["--help"] else id) getOpts
  optionHandler opts

optionHandler :: Bumper -> IO ()
optionHandler Bumper{..} = do
  -- file list feature needs to be added here, exec should not be responsible for
  -- anything else beside working on file
  sequence' (map (\file -> do
                    exec part current_version file
                 ) files)

-- Run all IO actions from list in sequence
sequence' :: [IO ()] -> IO()
sequence' [] = return ()
sequence' (x:xs) = do
  x
  sequence' xs

exec :: FilePath -> [Char] -> [Char] -> IO ()
exec part current_version file = do
  contents <- readFile file
  let fileLines = lines contents
      bumpedVer = versionBumper part (splitVersion current_version)
      newContent = unlines (map (\x -> replace current_version bumpedVer x) fileLines)
  when (length newContent > 0) $
       writeFile file newContent
