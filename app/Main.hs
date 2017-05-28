module Main where

import Kolo

main :: IO ()
main = do
  let s = newServer defaultConfig
  serve s
