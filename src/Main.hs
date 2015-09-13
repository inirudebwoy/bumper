#! /usr/bin/env runhaskell
{-# LANGUAGE RecordWildCards #-}

module Main where

import Bumper
import System.Environment
import System.Console.CmdArgs
import Control.Monad (when)

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

exec :: Part -> [Char] -> FilePath -> IO ()
exec part current_version file = do
  contents <- readFile file
  let fileLines = lines contents
      bumpedVer = versionBumper part (splitVersion current_version)
      newContent = unlines (map (\x -> replace current_version bumpedVer x) fileLines)
  when (length newContent > 0) $
       writeFile file newContent
