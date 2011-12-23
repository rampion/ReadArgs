{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List
import ReadArgs

main = do 
  ( n :: Int, 
    b :: Maybe String, 
    xs :: [String]) <- readArgs
  print (n,b,xs)
