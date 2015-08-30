module Main where

import Bumper

main :: IO ()
main = do
  args <- getArgs
  opts <- (if null args then withArgs ["--help"] else id) getOpts
  optionHandler opts
