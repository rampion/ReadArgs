{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List
import ReadArgs

main = do 
  (a :: Int) :& () <- readArgs
  print a
