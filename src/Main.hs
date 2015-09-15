#! /usr/bin/env runhaskell
{-# LANGUAGE RecordWildCards #-}

module Main where

import Bumper
import System.Environment
import System.Console.CmdArgs
import Control.Monad (when)

getOpts :: Mode (CmdArgs Bumper)
getOpts = cmdArgsMode $ modes [major, minor, patch]
          &= help ("Version bumping software.")
          &= summary "bumper 0.0.1, Michał Klich"

main :: IO ()
main = do
  callArgs <- getArgs
  opts <- (if null callArgs then withArgs ["--help"] else id) $ cmdArgsRun getOpts
  optionHandler opts

-- file list feature needs to be added here, exec should not be responsible for
-- anything else beside working on file
optionHandler :: Bumper -> IO ()
optionHandler Major{..} = do
  -- factor out this to a separate function so part can be passed as argument?
  sequence' (map (\file -> do
                    exec MajorPart current_version file
                 ) files)
optionHandler Minor{..} = do
  sequence' (map (\file -> do
                    exec MinorPart current_version file
                 ) files)
optionHandler Patch{..} = do
  sequence' (map (\file -> do
                    exec PatchPart current_version file
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
