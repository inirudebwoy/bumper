#! /usr/bin/env runhaskell
{-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where

import System.Console.CmdArgs

data Bumper = Bumper {current_version :: String} deriving (Show, Data, Typeable)

bumper = Bumper {
         current_version = def &= help "The current version of the software package before bumping."
         }
         &= help ("Version bumping software.")
         &= summary "bumper v0.0.1, Michał Klich"


main = cmdArgs bumper
