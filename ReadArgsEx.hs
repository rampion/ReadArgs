{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List
import ReadArgs

main = do 
  ( Require n :: Require Int, 
    b :: Maybe String, 
    xs :: [String]) <- readArgs
  print (n,b,xs)
