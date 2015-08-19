#! /usr/bin/env runhaskell
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Main (main) where

import System.Console.CmdArgs
import System.Environment


data Bumper = Bumper {current_version :: String, part :: String, file :: String}
              deriving (Show, Data, Typeable)

bumper :: Bumper
bumper = Bumper {
         current_version = def &= help "The current version of the software package before bumping." &= typ "VERSION",
         part = def &= argPos 0 &= typ "PART",
         file = def &= argPos 1 &= typFile}
         &= help ("Version bumping software.")
         &= summary "bumper v0.0.1, Michał Klich"

getOpts :: IO Bumper
getOpts = cmdArgs $ bumper
          -- add additional arguments

-- bump :: String -> String -> String -> IO ()
-- bump version part file = do
--   contents <- readFile file
--   putStrLn $ version ++ part ++ file

-- bump' :: Bumper -> String
-- bump' (Bumper {current_version = v, part = p, file = f}) = (v ++( p ++ f))

exec :: Bumper -> IO ()
exec Bumper{..} = do
  -- do work here
  putStrLn $ "Hey, " ++ current_version

optionHandler :: Bumper -> IO ()
optionHandler opts@Bumper{..} = do
  -- cleanup arguments
  exec opts

main :: IO ()
main = do
  args <- getArgs
  opts <- (if null args then withArgs ["--help"] else id) getOpts
  optionHandler opts
