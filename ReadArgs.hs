{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, UndecidableInstances, OverlappingInstances, TypeOperators #-}
module ReadArgs where

import Control.Arrow (first)

import Data.Maybe 
import Data.List 
import Data.Typeable 

import System.Environment
import System.Exit
import System.IO

-- |parse the desired argument tuple from the command line or 
--  print a simple usage statment and quit
readArgs :: ArgumentTuple a => IO a
readArgs = getArgs >>= readArgsFrom

-- |read args from the given strings or 
--  print a simple usage statment and quit
--  (so you can do option parsing first)
readArgsFrom :: ArgumentTuple a => [String] -> IO a
readArgsFrom ss = 
  let ma@(~(Just a)) = parseArgsFrom ss 
  in case ma of 
    Nothing -> do 
      progName <- getProgName
      hPutStrLn stderr $ "usage: " ++ progName ++ usageFor a
      exitFailure
    _ -> return a

-- |a class for types that can be parsed from exactly one command line argument
class Arguable a where
  parse :: String -> Maybe a
  -- |name's argument will usually be undefined, so when defining instances of
  -- Arguable, it should be lazy in its argument
  name :: a -> String

-- |all types that are typeable and readable can be used as simple arguments
instance (Typeable t, Read t) => Arguable t where
  parse s = case reads s of
    [(i,"")] -> Just i
    _ -> Nothing
  name t = showsTypeRep (typeOf t) ""

-- |string is a special case, so that we don't force the user to double-quote
-- their input
instance Arguable String where
  parse = Just
  name _ = "String"

-- |char is a special case, so that we don't force the user to single-quote
-- their input
instance Arguable Char where
  parse [x] = Just x
  parse _ = Nothing
  name _ = "Char"

-- |a class for types that can be parsed from some number of command line
-- arguments
class Argument a where
  parseArg :: [String] -> [(a, [String])]
  -- |argName's argument will usually be undefined, so when defining instances of
  -- Arguable, it should be lazy in its argument
  argName :: a -> String

-- |use the arguable tyep to just parse a single argument
instance Arguable a => Argument a where
  parseArg [] = []
  parseArg (s:ss) = do
    a <- maybeToList $ parse s
    return (a, ss)
  argName = name

-- |use Maybe when it should be parsed from one or zero (greedily)
instance Arguable a => Argument (Maybe a) where
  argName ~(Just x) = "["++name x++"]"
  parseArg [] = [(Nothing, [])]
  parseArg ss'@(s:ss) = case parse s of
    Nothing -> [(Nothing, ss')]
    justA   -> [(justA, ss),(Nothing,ss')]

-- |use a list when it should be parsed from zero or more (greedily)
instance Arguable a => Argument [a] where
  argName ~(x:_) = "["++name x ++"...]"
  parseArg ss = reverse $ inits ss' `zip` tails ss
    where ss' = map fromJust . takeWhile isJust $ map parse ss

-- |a wrapper type to indicate a non-greedy list or maybe
newtype NonGreedy m a = NonGreedy { unNonGreedy :: m a } deriving (Show, Eq)
-- |use NonGreedy when it should be parsed non-greedily
--  (e.g. @(NonGreedy xs :: NonGreedy [] Int, x :: Maybe Float) <- readArgs@)
instance Argument (m a) => Argument (NonGreedy m a) where
  argName ~(NonGreedy m) = argName m
  parseArg = map (first NonGreedy) . reverse . parseArg

-- |make sure strings are handled as a separate type, not a list of chars
instance Argument String where
  parseArg [] = []
  parseArg (s:ss) = do
    a <- maybeToList $ parse s
    return (a, ss)
  argName = name

-- |a class for tuples of types that can be parsed from the entire list
-- of arguments
class ArgumentTuple a where
  parseArgsFrom :: [String] -> Maybe a
  -- |usageFor's argument will usually be undefined, so when defining instances of
  -- Arguable, it should be lazy in its argument
  usageFor :: a -> String

-- |use () for no arguments
instance ArgumentTuple () where
  parseArgsFrom [] = Just ()
  parseArgsFrom _ = Nothing
  usageFor = const ""

-- |use :& to construct arbitrary length tuples of any parsable arguments
data a :& b = a :& b deriving (Show, Eq)
infixr 5 :&
instance (Argument a, ArgumentTuple y) => ArgumentTuple (a :& y) where
  parseArgsFrom ss = listToMaybe $ do
    (a, ss') <- parseArg ss
    y <- maybeToList $ parseArgsFrom ss'
    return $ a :& y
  usageFor ~(a :& y) = " " ++ argName a  ++ usageFor y

-- Use :& to derive an instance for single arguments
instance (Argument a) => ArgumentTuple a where
  parseArgsFrom ss = do
    a :& () <- parseArgsFrom ss
    return a
  usageFor a = usageFor (a :& ())

-- Use :& to derive instances for all the normal tuple types
instance (Argument b, Argument a) => ArgumentTuple (b,a) where
  parseArgsFrom ss = do
    b :& a :& () <- parseArgsFrom ss
    return (b,a)
  usageFor ~(b,a) = usageFor (b :& a :& ())

instance (Argument c, Argument b, Argument a) => ArgumentTuple (c,b,a) where
  parseArgsFrom ss = do
    c :& b :& a :& () <- parseArgsFrom ss
    return (c,b,a)
  usageFor ~(c,b,a) = usageFor (c :& b :& a :& ())

instance (Argument d, Argument c, Argument b, Argument a) => ArgumentTuple (d,c,b,a) where
  parseArgsFrom ss = do
    d :& c :& b :& a :& () <- parseArgsFrom ss
    return (d,c,b,a)
  usageFor ~(d,c,b,a) = usageFor (d :& c :& b :& a :& ())

instance (Argument e, Argument d, Argument c, Argument b, Argument a) => ArgumentTuple (e,d,c,b,a) where
  parseArgsFrom ss = do
    e :& d :& c :& b :& a :& () <- parseArgsFrom ss
    return (e,d,c,b,a)
  usageFor ~(e,d,c,b,a) = usageFor (e :& d :& c :& b :& a :& ())

instance (Argument f, Argument e, Argument d, Argument c, Argument b, Argument a) => ArgumentTuple (f,e,d,c,b,a) where
  parseArgsFrom ss = do
    f :& e :& d :& c :& b :& a :& () <- parseArgsFrom ss
    return (f,e,d,c,b,a)
  usageFor ~(f,e,d,c,b,a) = usageFor (f :& e :& d :& c :& b :& a :& ())

instance (Argument g, Argument f, Argument e, Argument d, Argument c, Argument b, Argument a) => ArgumentTuple (g,f,e,d,c,b,a) where
  parseArgsFrom ss = do
    g :& f :& e :& d :& c :& b :& a :& () <- parseArgsFrom ss
    return (g,f,e,d,c,b,a)
  usageFor ~(g,f,e,d,c,b,a) = usageFor (g :& f :& e :& d :& c :& b :& a :& ())

instance (Argument h, Argument g, Argument f, Argument e, Argument d, Argument c, Argument b, Argument a) => ArgumentTuple (h,g,f,e,d,c,b,a) where
  parseArgsFrom ss = do
    h :& g :& f :& e :& d :& c :& b :& a :& () <- parseArgsFrom ss
    return (h,g,f,e,d,c,b,a)
  usageFor ~(h,g,f,e,d,c,b,a) = usageFor (h :& g :& f :& e :& d :& c :& b :& a :& ())

instance (Argument i, Argument h, Argument g, Argument f, Argument e, Argument d, Argument c, Argument b, Argument a) => ArgumentTuple (i,h,g,f,e,d,c,b,a) where
  parseArgsFrom ss = do
    i :& h :& g :& f :& e :& d :& c :& b :& a :& () <- parseArgsFrom ss
    return (i,h,g,f,e,d,c,b,a)
  usageFor ~(i,h,g,f,e,d,c,b,a) = usageFor (i :& h :& g :& f :& e :& d :& c :& b :& a :& ())

instance (Argument j, Argument i, Argument h, Argument g, Argument f, Argument e, Argument d, Argument c, Argument b, Argument a) => ArgumentTuple (j,i,h,g,f,e,d,c,b,a) where
  parseArgsFrom ss = do
    j :& i :& h :& g :& f :& e :& d :& c :& b :& a :& () <- parseArgsFrom ss
    return (j,i,h,g,f,e,d,c,b,a)
  usageFor ~(j,i,h,g,f,e,d,c,b,a) = usageFor (j :& i :& h :& g :& f :& e :& d :& c :& b :& a :& ())

instance (Argument k, Argument j, Argument i, Argument h, Argument g, Argument f, Argument e, Argument d, Argument c, Argument b, Argument a) => ArgumentTuple (k,j,i,h,g,f,e,d,c,b,a) where
  parseArgsFrom ss = do
    k :& j :& i :& h :& g :& f :& e :& d :& c :& b :& a :& () <- parseArgsFrom ss
    return (k,j,i,h,g,f,e,d,c,b,a)
  usageFor ~(k,j,i,h,g,f,e,d,c,b,a) = usageFor (k :& j :& i :& h :& g :& f :& e :& d :& c :& b :& a :& ())

instance (Argument l, Argument k, Argument j, Argument i, Argument h, Argument g, Argument f, Argument e, Argument d, Argument c, Argument b, Argument a) => ArgumentTuple (l,k,j,i,h,g,f,e,d,c,b,a) where
  parseArgsFrom ss = do
    l :& k :& j :& i :& h :& g :& f :& e :& d :& c :& b :& a :& () <- parseArgsFrom ss
    return (l,k,j,i,h,g,f,e,d,c,b,a)
  usageFor ~(l,k,j,i,h,g,f,e,d,c,b,a) = usageFor (l :& k :& j :& i :& h :& g :& f :& e :& d :& c :& b :& a :& ())

instance (Argument m, Argument l, Argument k, Argument j, Argument i, Argument h, Argument g, Argument f, Argument e, Argument d, Argument c, Argument b, Argument a) => ArgumentTuple (m,l,k,j,i,h,g,f,e,d,c,b,a) where
  parseArgsFrom ss = do
    m :& l :& k :& j :& i :& h :& g :& f :& e :& d :& c :& b :& a :& () <- parseArgsFrom ss
    return (m,l,k,j,i,h,g,f,e,d,c,b,a)
  usageFor ~(m,l,k,j,i,h,g,f,e,d,c,b,a) = usageFor (m :& l :& k :& j :& i :& h :& g :& f :& e :& d :& c :& b :& a :& ())

instance (Argument n, Argument m, Argument l, Argument k, Argument j, Argument i, Argument h, Argument g, Argument f, Argument e, Argument d, Argument c, Argument b, Argument a) => ArgumentTuple (n,m,l,k,j,i,h,g,f,e,d,c,b,a) where
  parseArgsFrom ss = do
    n :& m :& l :& k :& j :& i :& h :& g :& f :& e :& d :& c :& b :& a :& () <- parseArgsFrom ss
    return (n,m,l,k,j,i,h,g,f,e,d,c,b,a)
  usageFor ~(n,m,l,k,j,i,h,g,f,e,d,c,b,a) = usageFor (n :& m :& l :& k :& j :& i :& h :& g :& f :& e :& d :& c :& b :& a :& ())

instance (Argument o, Argument n, Argument m, Argument l, Argument k, Argument j, Argument i, Argument h, Argument g, Argument f, Argument e, Argument d, Argument c, Argument b, Argument a) => ArgumentTuple (o,n,m,l,k,j,i,h,g,f,e,d,c,b,a) where
  parseArgsFrom ss = do
    o :& n :& m :& l :& k :& j :& i :& h :& g :& f :& e :& d :& c :& b :& a :& () <- parseArgsFrom ss
    return (o,n,m,l,k,j,i,h,g,f,e,d,c,b,a)
  usageFor ~(o,n,m,l,k,j,i,h,g,f,e,d,c,b,a) = usageFor (o :& n :& m :& l :& k :& j :& i :& h :& g :& f :& e :& d :& c :& b :& a :& ())
