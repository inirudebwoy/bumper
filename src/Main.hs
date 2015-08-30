module Main where

import Bumper
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  opts <- (if null args then withArgs ["--help"] else id) getOpts
  optionHandler opts
