#! /usr/bin/env runhaskell

module Bumper where

import Data.List

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
