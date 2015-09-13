#! /usr/bin/env runhaskell
{-# LANGUAGE DeriveDataTypeable #-}

module Bumper where

import Data.List
import System.Console.CmdArgs

data Part = Major | Minor | Patch
          deriving (Data, Typeable, Show, Eq)

data Bumper = Bumper
    {current_version :: String,
     suffix :: String,
     build :: String,
     part :: Part,
     files :: [FilePath]
    } deriving (Show, Data, Typeable)

bumper :: Bumper
bumper = Bumper
         {current_version = def &= help "The current version of the software package before bumping." &= typ "VERSION",
          suffix = def &= help "Suffix to be added, e.g., alpha, rc-2" &= typ "SUFFIX",
          build = def &= help "Build number to be added, e.g., b42, f7a8051" &= typ "BUILD",
          part = enum
                 [Major &= help "Major",
                  Minor &= help "Minor",
                  Patch &= help "Patch"
                 ],
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
partIndex Major = 0
partIndex Minor = 1
partIndex Patch = 2

versionBumper :: Part -> [[Char]] -> [Char]
versionBumper part current = makeVersion (map (\(x, y) -> if y == partIndex part
                                                          then bumpElement x
                                                          else x) (zip current [0..]))
    where
      bumpElement el = show ((read el :: Int) + 1) :: String
