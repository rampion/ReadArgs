{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}
module ReadArgs where

import Control.Monad.Identity
import Data.Typeable
import System.Environment
import System.Exit
import System.IO

newtype UnquotedString = Unquoted String deriving (Show, Eq, Typeable)

instance Read UnquotedString where
  readsPrec _ s = [(Unquoted s,"")]

class ArgTuple at where 
  types :: at -> [TypeRep]
  useArgs :: ArgTuple at' => at' -> [String] -> IO at
  readArgs :: ArgTuple at => IO at


usage :: ArgTuple at => at -> IO a
usage at = do
  progName <- getProgName
  hPutStrLn stderr $ foldl (\u tr -> u ++ " <" ++ showsTypeRep tr ">") ("usage: " ++ progName) (types at)
  exitFailure

instance (Typeable a, Read a) => ArgTuple (Identity a) where
  types _ = [typeOf (undefined :: a)]
  useArgs _ [s] | [(a,"")] <- reads s = return (Identity a)
  useArgs at _ = usage at
  readArgs = getArgs >>= useArgs (undefined :: Identity a)

instance (Typeable a, Read a, Typeable b, Read b) => ArgTuple (a,b) where
  types _ = typeOf (undefined :: a) : types (undefined :: Identity b)
  useArgs at (s:ss) | [(a,"")] <- reads s = do
    Identity b <- useArgs at ss
    return (a, b)
  useArgs at _ = usage at
  readArgs = getArgs >>= useArgs (undefined :: (a,b))

instance (Typeable a, Read a, Typeable b, Read b, Typeable c, Read c) => ArgTuple (a,b,c) where
  types _ = typeOf (undefined :: a) : types (undefined :: (b,c))
  useArgs at (s:ss) | [(a,"")] <- reads s = do
    (b,c) <- useArgs at ss
    return (a, b, c)
  useArgs at _ = usage at
  readArgs = getArgs >>= useArgs (undefined :: (a,b,c))
