{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.List
import ReadArgs

main = do 
  ( a :: Int, 
    b :: Maybe Bool, 
    cs :: [Bool],
    d :: Maybe Bool,
    e :: Bool) <- readArgs
  print (a,b,cs,d,e)
