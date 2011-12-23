{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad.Identity
import Data.List
import ReadArgs

main = do 
  (n :: Int, Unquoted b, Unquoted s) <- readArgs
  putStrLn . intercalate b $ replicate n s
