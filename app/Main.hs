module Main where

import Kolo

main :: IO ()
main = do
  s <- newServer defaultConfig
  serve s
