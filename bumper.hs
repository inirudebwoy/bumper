#! /usr/bin/env runhaskell
{-# LANGUAGE DeriveDataTypeable #-}

module Main (main) where

import System.IO
import System.Console.CmdArgs

data Bumper = Bumper {current_version :: String, part :: String, file :: String}
              deriving (Show, Data, Typeable)

bumper = Bumper {
         current_version = def &= help "The current version of the software package before bumping." &= typ "VERSION",
         part = def &= argPos 0 &= typ "PART",
         file = def &= argPos 1 &= typFile}
         &= help ("Version bumping software.")
         &= summary "bumper v0.0.1, Michał Klich"
