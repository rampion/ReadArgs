{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List
import ReadArgs

main = do 
  ( a :: Int, 
    b :: Maybe String, 
    cs :: [String],
    d :: Maybe String,
    e :: String) <- readArgs
  print (a,b,cs,d,e)
