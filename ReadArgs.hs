{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances, OverlappingInstances, TypeOperators #-}
module ReadArgs where

import Control.Arrow (first)

import Data.Maybe 
import Data.List 
import Data.Typeable 

import System.Environment
import System.Exit
import System.IO

-- parse the desired argument tuple from the command line or 
-- print a simple usage statment and quit
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

-- use the arguable tyep to just parse a single argument
instance Arguable a => Argument a where
  parseArg [] = []
  parseArg (s:ss) = do
    a <- maybeToList $ parse s
    return (a, ss)
  argName = name

-- use Maybe when it should be parsed from one or zero (greedily)
instance Arguable a => Argument (Maybe a) where
  argName ~(Just x) = "["++name x++"]"
  parseArg [] = [(Nothing, [])]
  parseArg ss'@(s:ss) = case parse s of
    Nothing -> [(Nothing, ss')]
    justA   -> [(justA, ss),(Nothing,ss')]

-- use a list when it should be parsed from zero or more (greedily)
instance Arguable a => Argument [a] where
  argName ~(x:_) = "["++name x ++"...]"
  parseArg ss = reverse $ inits ss' `zip` tails ss
    where ss' = map fromJust . takeWhile isJust $ map parse ss

-- use NonGreedy when it should be parsed non-greedily
newtype NonGreedy m a = NonGreedy { unNonGreedy :: m a }
instance Argument (m a) => Argument (NonGreedy m a) where
  argName ~(NonGreedy m) = argName m
  parseArg = map (first NonGreedy) . reverse . parseArg

-- make sure strings are handled as a separate type, not a list of chars
instance Argument String where
  parseArg [] = []
  parseArg (s:ss) = do
    a <- maybeToList $ parse s
    return (a, ss)
  argName = name

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

instance (Argument e, Argument d, Argument c, Argument b, Argument a) => ArgumentTuple (e,d,c,b,a) where
  readArgsFrom ss = do
    e :& d :& c :& b :& a :& () <- readArgsFrom ss
    return (e,d,c,b,a)
  usage ~(e,d,c,b,a) = usage (e :& d :& c :& b :& a :& ())

instance (Argument f, Argument e, Argument d, Argument c, Argument b, Argument a) => ArgumentTuple (f,e,d,c,b,a) where
  readArgsFrom ss = do
    f :& e :& d :& c :& b :& a :& () <- readArgsFrom ss
    return (f,e,d,c,b,a)
  usage ~(f,e,d,c,b,a) = usage (f :& e :& d :& c :& b :& a :& ())

instance (Argument g, Argument f, Argument e, Argument d, Argument c, Argument b, Argument a) => ArgumentTuple (g,f,e,d,c,b,a) where
  readArgsFrom ss = do
    g :& f :& e :& d :& c :& b :& a :& () <- readArgsFrom ss
    return (g,f,e,d,c,b,a)
  usage ~(g,f,e,d,c,b,a) = usage (g :& f :& e :& d :& c :& b :& a :& ())

instance (Argument h, Argument g, Argument f, Argument e, Argument d, Argument c, Argument b, Argument a) => ArgumentTuple (h,g,f,e,d,c,b,a) where
  readArgsFrom ss = do
    h :& g :& f :& e :& d :& c :& b :& a :& () <- readArgsFrom ss
    return (h,g,f,e,d,c,b,a)
  usage ~(h,g,f,e,d,c,b,a) = usage (h :& g :& f :& e :& d :& c :& b :& a :& ())

instance (Argument i, Argument h, Argument g, Argument f, Argument e, Argument d, Argument c, Argument b, Argument a) => ArgumentTuple (i,h,g,f,e,d,c,b,a) where
  readArgsFrom ss = do
    i :& h :& g :& f :& e :& d :& c :& b :& a :& () <- readArgsFrom ss
    return (i,h,g,f,e,d,c,b,a)
  usage ~(i,h,g,f,e,d,c,b,a) = usage (i :& h :& g :& f :& e :& d :& c :& b :& a :& ())

instance (Argument j, Argument i, Argument h, Argument g, Argument f, Argument e, Argument d, Argument c, Argument b, Argument a) => ArgumentTuple (j,i,h,g,f,e,d,c,b,a) where
  readArgsFrom ss = do
    j :& i :& h :& g :& f :& e :& d :& c :& b :& a :& () <- readArgsFrom ss
    return (j,i,h,g,f,e,d,c,b,a)
  usage ~(j,i,h,g,f,e,d,c,b,a) = usage (j :& i :& h :& g :& f :& e :& d :& c :& b :& a :& ())

instance (Argument k, Argument j, Argument i, Argument h, Argument g, Argument f, Argument e, Argument d, Argument c, Argument b, Argument a) => ArgumentTuple (k,j,i,h,g,f,e,d,c,b,a) where
  readArgsFrom ss = do
    k :& j :& i :& h :& g :& f :& e :& d :& c :& b :& a :& () <- readArgsFrom ss
    return (k,j,i,h,g,f,e,d,c,b,a)
  usage ~(k,j,i,h,g,f,e,d,c,b,a) = usage (k :& j :& i :& h :& g :& f :& e :& d :& c :& b :& a :& ())

instance (Argument l, Argument k, Argument j, Argument i, Argument h, Argument g, Argument f, Argument e, Argument d, Argument c, Argument b, Argument a) => ArgumentTuple (l,k,j,i,h,g,f,e,d,c,b,a) where
  readArgsFrom ss = do
    l :& k :& j :& i :& h :& g :& f :& e :& d :& c :& b :& a :& () <- readArgsFrom ss
    return (l,k,j,i,h,g,f,e,d,c,b,a)
  usage ~(l,k,j,i,h,g,f,e,d,c,b,a) = usage (l :& k :& j :& i :& h :& g :& f :& e :& d :& c :& b :& a :& ())

instance (Argument m, Argument l, Argument k, Argument j, Argument i, Argument h, Argument g, Argument f, Argument e, Argument d, Argument c, Argument b, Argument a) => ArgumentTuple (m,l,k,j,i,h,g,f,e,d,c,b,a) where
  readArgsFrom ss = do
    m :& l :& k :& j :& i :& h :& g :& f :& e :& d :& c :& b :& a :& () <- readArgsFrom ss
    return (m,l,k,j,i,h,g,f,e,d,c,b,a)
  usage ~(m,l,k,j,i,h,g,f,e,d,c,b,a) = usage (m :& l :& k :& j :& i :& h :& g :& f :& e :& d :& c :& b :& a :& ())

instance (Argument n, Argument m, Argument l, Argument k, Argument j, Argument i, Argument h, Argument g, Argument f, Argument e, Argument d, Argument c, Argument b, Argument a) => ArgumentTuple (n,m,l,k,j,i,h,g,f,e,d,c,b,a) where
  readArgsFrom ss = do
    n :& m :& l :& k :& j :& i :& h :& g :& f :& e :& d :& c :& b :& a :& () <- readArgsFrom ss
    return (n,m,l,k,j,i,h,g,f,e,d,c,b,a)
  usage ~(n,m,l,k,j,i,h,g,f,e,d,c,b,a) = usage (n :& m :& l :& k :& j :& i :& h :& g :& f :& e :& d :& c :& b :& a :& ())

instance (Argument o, Argument n, Argument m, Argument l, Argument k, Argument j, Argument i, Argument h, Argument g, Argument f, Argument e, Argument d, Argument c, Argument b, Argument a) => ArgumentTuple (o,n,m,l,k,j,i,h,g,f,e,d,c,b,a) where
  readArgsFrom ss = do
    o :& n :& m :& l :& k :& j :& i :& h :& g :& f :& e :& d :& c :& b :& a :& () <- readArgsFrom ss
    return (o,n,m,l,k,j,i,h,g,f,e,d,c,b,a)
  usage ~(o,n,m,l,k,j,i,h,g,f,e,d,c,b,a) = usage (o :& n :& m :& l :& k :& j :& i :& h :& g :& f :& e :& d :& c :& b :& a :& ())
