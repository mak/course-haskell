{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module List2 where

import Data.Monoid 

class Monoid m  => MonCat a m  where
   moncat :: m -> a

instance Monoid m => MonCat m  m where
   moncat = id 

instance (Monoid m, MonCat r m) => MonCat ( m -> r ) m where 
   moncat e  = moncat . (e `mappend` )


class Show a => IsAllowed a  
instance IsAllowed Int
instance IsAllowed String
instance IsAllowed Float

class Args a where
   pr :: [String] -> [Format] -> a

instance Args String where
   pr acc desc = concat . reverse $ foldl f acc desc
    where f acc (FLit s) = s :acc
	  f acc _ = error "Brakuje argumentow"  

instance (IsAllowed a, Args r) => Args (a -> r) where
   pr acc desc x = uncurry pr $ fmtx desc acc  where
    fmtx [] acc     = error "No formatting directive for the argument"
    fmtx (FLit s:desc) acc = fmtx desc (s:acc)
    fmtx (FFloat:desc) acc = (show x:acc,desc) 
    fmtx (FInt : desc) acc = (show x:acc,desc)
    fmtx (FStr : desc) acc = (unq( show x):acc,desc)
    unq ('"':str) | last str == '"' = init str
    unsq str = str

data Format = FLit String | FStr | FInt | FFloat deriving Show
sprintf :: Args a => String -> a
sprintf = pr [] . convert  
  where convert str = 
	  case break (=='%') str of
	     (s1,"") -> makeLit s1
	     (s1,'%':x:rest) | Just fd <- lookup x map  -> makeLit s1 ++ fd  : convert rest
	     (_,s2) -> error $ "bad descriptor: " ++ take 5 s2
	makeLit "" = []
	makeLit s = [FLit s]
	map = [('s',FStr),('d',FInt),('f',FFloat)]
