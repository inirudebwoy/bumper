#! /usr/bin/env runhaskell
{-# LANGUAGE DeriveDataTypeable #-}

module Bumper where

import Data.List
import System.Console.CmdArgs

data Part = MajorPart | MinorPart | PatchPart
           deriving (Data, Typeable, Show, Eq)

data Bumper =
    Major {current_version :: String,
            suffix :: String,
            build :: String,
            files :: [FilePath]
           }
    |
    Minor {current_version :: String,
           suffix :: String,
           build :: String,
           files :: [FilePath]
          }
    |
    Patch {current_version :: String,
           suffix :: String,
           build :: String,
           files :: [FilePath]
          }
      deriving (Show, Data, Typeable)

-- If data types can't share arguments text need to be kept as variables and reused
text :: String
text = "The current version of the software package before bumping."


-- there needs to be way to share these arguments across all data types
-- like Major, Minor, Patch
major :: Bumper
major = Major
         {current_version = def &= help text &= typ "VERSION",
          suffix = def &= help "Suffix to be added, e.g., alpha, rc-2" &= typ "SUFFIX",
          build = def &= help "Build number to be added, e.g., b42, f7a8051" &= typ "BUILD",
          files = def &= args &= typ "FILES/DIRS"
         }
        &= details ["Major mode!!"]

minor :: Bumper
minor =  Minor
         {current_version = def &= help "The current version of the software package before bumping." &= typ "VERSION",
          suffix = def &= help "Suffix to be added, e.g., alpha, rc-2" &= typ "SUFFIX",
          build = def &= help "Build number to be added, e.g., b42, f7a8051" &= typ "BUILD",
          files = def &= args &= typ "FILES/DIRS"
         }

patch :: Bumper
patch = Patch
        {current_version = def &= help "The current version of the software package before bumping." &= typ "VERSION",
          suffix = def &= help "Suffix to be added, e.g., alpha, rc-2" &= typ "SUFFIX",
          build = def &= help "Build number to be added, e.g., b42, f7a8051" &= typ "BUILD",
          files = def &= args &= typ "FILES/DIRS"
         }

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
addSuffix ver suf =
    addBuild (takeWhile ('-' <) ver ++ "-" ++ suf) (takeBuild ver)

addBuild :: [Char] -> [Char] -> [Char]
addBuild version "" = takeWhile ('+' <) version
addBuild version build = takeWhile ('+' <) version ++ "+" ++ build

partIndex :: Part -> Int
partIndex MajorPart = 0
partIndex MinorPart = 1
partIndex PatchPart = 2

versionBumper :: Part -> [[Char]] -> [Char]
versionBumper part current = makeVersion (map (\(x, y) -> if y == partIndex part
                                                          then bumpElement x
                                                          else x) (zip current [0..]))
    where
      bumpElement el = show ((read el :: Int) + 1) :: String
