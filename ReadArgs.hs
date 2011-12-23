{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances, OverlappingInstances, TypeOperators #-}
module ReadArgs where

import Data.Maybe 
import Data.List 
import Data.Typeable 

import System.Environment
import System.Exit
import System.IO

-- parse the desired argument tuple from the command line or print
-- a simple usage statment
readArgs :: ArgumentTuple a => IO a
readArgs = do
  as@(~(a:_)) <- readArgsFrom `fmap` getArgs
  case as of 
    [] -> do 
      progName <- getProgName
      hPutStrLn stderr $ "usage: " ++ progName ++ usage a
      exitFailure
    _ -> return a

-- a class for types that can be parsed from exactly one command line argument
class Arguable a where
  parse :: String -> Maybe a
  name :: a -> String

-- all types that are typeable and readable can be used as simple arguments
instance (Typeable t, Read t) => Arguable t where
  parse s = case reads s of
    [(i,"")] -> Just i
    otherwise -> Nothing
  name t = showsTypeRep (typeOf t) ""

-- string is a special case, so that we don't force the user to double-quote
-- their input
instance Arguable String where
  parse = Just
  name _ = "String"

-- a class for types that can be parsed from some number of command line
-- arguments
class Argument a where
  parseArg :: [String] -> [(a, [String])]
  argName :: a -> String

instance Arguable a => Argument a where
  parseArg [] = []
  parseArg (s:ss) = do
    a <- maybeToList $ parse s
    return (a, ss)
  argName = name

-- use Maybe when it should be parsed from zero or one
instance Arguable a => Argument (Maybe a) where
  argName ~(Just x) = "["++name x++"]"
  parseArg [] = [(Nothing, [])]
  parseArg ss'@(s:ss) = case parse s of
    Nothing -> [(Nothing, ss')]
    justA   -> [(justA, ss)]

-- use a list when it should be parsed from zero or more
instance Arguable a => Argument [a] where
  argName ~(x:_) = "["++name x ++"...]"
  parseArg ss = inits ss' `zip` tails ss
    where ss' = map fromJust . takeWhile isJust $ map parse ss

-- a class for tuples of types that can be parsed from the entire list
-- of arguments
class ArgumentTuple a where
  readArgsFrom :: [String] -> [a]
  usage :: a -> String

-- use () for no arguments
instance ArgumentTuple () where
  readArgsFrom [] = [()]
  readArgsFrom _ = []
  usage = const ""

-- use :& to construct arbitrary length tuples of any parsable arguments
data a :& b = a :& b
infixr 5 :&
instance (Argument a, ArgumentTuple y) => ArgumentTuple (a :& y) where
  readArgsFrom ss = do
    (a, ss') <- parseArg ss
    y <- readArgsFrom ss'
    return $ a :& y
  usage ~(a :& y) = " " ++ argName a  ++ usage y

-- Use :& to derive instances for all the normal tuple types
instance (Argument b, Argument a) => ArgumentTuple (b,a) where
  readArgsFrom ss = do
    b :& a :& () <- readArgsFrom ss
    return (b,a)
  usage ~(b,a) = usage (b :& a :& ())

instance (Argument c, Argument b, Argument a) => ArgumentTuple (c,b,a) where
  readArgsFrom ss = do
    c :& b :& a :& () <- readArgsFrom ss
    return (c,b,a)
  usage ~(c,b,a) = usage (c :& b :& a :& ())

instance (Argument d, Argument c, Argument b, Argument a) => ArgumentTuple (d,c,b,a) where
  readArgsFrom ss = do
    d :& c :& b :& a :& () <- readArgsFrom ss
    return (d,c,b,a)
  usage ~(d,c,b,a) = usage (d :& c :& b :& a :& ())
