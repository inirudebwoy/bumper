#! /usr/bin/env runhaskell
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Bumper where

import System.Console.CmdArgs
import Data.List


data Bumper = Bumper {current_version :: String, part :: String, file :: String}
              deriving (Show, Data, Typeable)

bumper :: Bumper
bumper = Bumper {current_version = def &= help "The current version of the software package before bumping." &= typ "VERSION",
                 part = def &= argPos 0 &= typ "PART",
                 file = def &= argPos 1 &= typFile}

getOpts :: IO Bumper
getOpts = cmdArgs $ bumper
          &= help ("Version bumping software.")
          &= summary "bumper v0.0.1, Michał Klich"

-- add additional arguments

replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace old new xs@(y:ys) =
    case stripPrefix old xs of
      Nothing -> y : replace old new ys
      Just ys' -> new ++ replace old new ys'

splitVersion :: [Char] -> [[Char]]
splitVersion [] = []
splitVersion v@(x:xs)
    | isDot x = splitVersion xs
    | otherwise = let (h, t) = break isDot v in h:(splitVersion t)
    where isDot x' = x' == '.'

makeVersion :: [[Char]] -> [Char]
makeVersion [] = []
makeVersion chunks = intercalate "." chunks

versionBumper :: [Char] -> [[Char]] -> [Char]
versionBumper part current
    | part == "major" && length current >= 1 =
        makeVersion ([bumpElement (current !! 0)] ++ tail current)
    | part == "minor" && length current >= 2 =
        makeVersion ([head current] ++ [bumpElement (current !! 1)] ++ [last current])
    | part == "patch" && length current >= 3 =
        makeVersion (init current ++ [bumpElement (current !! 2)])
    | otherwise = makeVersion current -- error here?
    where bumpElement el = show ((read el :: Int) + 1) :: String

exec :: Bumper -> IO ()
exec Bumper{..} = do
  contents <- readFile file
  let fileLines = lines contents
      bumpedVer = versionBumper part (splitVersion current_version)
  putStr $ unlines (map (\x -> replace current_version bumpedVer x) fileLines)
  -- do work here

optionHandler :: Bumper -> IO ()
optionHandler opts@Bumper{..} = do
  -- cleanup arguments
  exec opts
