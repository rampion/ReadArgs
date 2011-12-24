{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import ReadArgs

-- try running this  with a couple sample inputs
--  % ReadArgsEx hello x
--  (Nothing, "hello", [], 'x')
--  % ReadArgsEx a hello x
--  (Just 'a', "hello", [], 'x')
--  % ReadArgsEx hello 1 2 3 x
--  (Nothing, "hello", [1,2,3], 'x')
--  % ReadArgsEx a hello 1 2 3 x
--  (Just 'a', "hello", [1,2,3], 'x')
main = do 
  (a :: Maybe Char, b :: String, c :: [Int], d :: Char) <- readArgs
  print (a,b,c,d)
