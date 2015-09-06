#! /usr/bin/env runhaskell
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}

module Bumper where

import Control.Monad (when)
import System.Console.CmdArgs
import Data.List


data Bumper = Bumper
    {current_version :: String,
     suffix :: String,
     build :: String,
     part :: String,
     file :: String
    } deriving (Show, Data, Typeable)

bumper :: Bumper
bumper = Bumper
         {current_version = def &= help "The current version of the software package before bumping." &= typ "VERSION",
          suffix = def &= help "Suffix to be added, e.g., alpha, rc-2" &= typ "SUFFIX",
          build = def &= help "Build number to be added, e.g., b42, f7a8051" &= typ "BUILD",
          part = def &= argPos 0 &= typ "PART",
          file = def &= argPos 1 &= typFile
         }

getOpts :: IO Bumper
getOpts = cmdArgs $ bumper
          &= help ("Version bumping software.")
          &= summary "bumper 0.0.1, Michał Klich"

-- add additional arguments

replace :: (Eq a) => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace old new xs@(y:ys) = case stripPrefix old xs of
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

takeBuild :: [Char] -> [Char]
takeBuild version = case dropWhile ('+' <) version of
                      "" -> ""
                      x -> tail x

addSuffix :: [Char] -> [Char] -> [Char]
addSuffix version suffix =
    addBuild (takeWhile ('-' <) version ++ "-" ++ suffix) (takeBuild version)

addBuild :: [Char] -> [Char] -> [Char]
addBuild version "" = takeWhile ('+' <) version
addBuild version build = takeWhile ('+' <) version ++ "+" ++ build

partIndex :: [Char] -> Int
partIndex part
        | part == "major" = 0
        | part == "minor" = 1
        | part == "patch" = 2
        | otherwise = -1 -- OMG, nope, this might throw error?

versionBumper :: [Char] -> [[Char]] -> [Char]
versionBumper part current = makeVersion (map (\(x, y) -> if y == partIndex part
                                                          then bumpElement x
                                                          else x) (zip current [0..]))
    where
      bumpElement el = show ((read el :: Int) + 1) :: String

exec :: Bumper -> IO ()
exec Bumper{..} = do
  contents <- readFile file
  let fileLines = lines contents
      bumpedVer = versionBumper part (splitVersion current_version)
      newContent = unlines (map (\x -> replace current_version bumpedVer x) fileLines)
  when (length newContent > 0) $
       writeFile file newContent

optionHandler :: Bumper -> IO ()
optionHandler opts@Bumper{..} = do
  -- cleanup arguments
  exec opts
