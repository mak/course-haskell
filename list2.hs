{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module List2 where

import Data.Monoid
import System.IO

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
	     (_,s2) -> error $ "bad descriptor: " ++ take 2 s2
	makeLit "" = []
	makeLit s = [FLit s]
	map = [('s',FStr),('d',FInt),('f',FFloat)]


newtype StateT s m a = StateT {runStateT :: s -> m (s,a) }

instance Monad m => Monad (StateT s m) where
   return a = StateT  $ \s -> return (s,a)
   m >>= k = StateT $ \s -> do
      (s1,a1)  <- runStateT m s 
      runStateT (k a1 ) s1

class MonadTrans (t :: (* -> *) -> (* -> *)) where
   lift :: Monad m => m a -> t m a 

instance MonadTrans (StateT s) where
   lift m = StateT $ \s -> m >>= return . (,) s

class Monad m => CountMonad m where
   counter :: m Int
   tick :: m ()

class Monad m => InputMonad m where
   getch :: m Char
   eof :: m Bool

class Monad m => InputCountMonad m where
   getc :: m Char
   eot :: m Bool
   position :: m Int

instance Monad m => CountMonad (StateT Int m) where
   counter = StateT $ \s -> return (s,s)
   tick = StateT $ \s -> return (s+1,())

instance Monad m => InputMonad (StateT String  m) where
   getch = StateT $ \(c:s) -> return (s,c)
   eof = StateT $ \s -> return (s,null s)

instance InputMonad m => InputMonad (StateT s m) where
   getch = lift getch 
   eof = lift eof

instance CountMonad m => CountMonad (StateT s m) where
   counter = lift counter
   tick = lift tick

instance (CountMonad m,InputMonad m) => InputCountMonad m where
   getc = tick >> getch
   eot = eof
   position = counter

lettersA :: InputCountMonad m => m [Int] 
lettersA = do 
  stop <- eot 
  if stop 
    then return [] 
    else do 
      ch <- getc 
      cnt <- position 
      rest <- lettersA 
      if ch == 'A' 
        then return (cnt : rest) 
        else return rest

lettersAinString2 :: StateT String (StateT Int Id) [Int] 
lettersAinString2 = lettersA

searchA :: String -> [Int]
searchA str = snd $ snd $ runId $ runStateT (runStateT lettersAinString2 str) 0

newtype Id a = Id {runId :: a }
instance Monad Id where
   return = Id
   m >>= k = k $ runId m 

instance InputMonad IO where
   getch = getChar
   eof = isEOF 

letterAIO :: StateT Int IO [Int]
letterAIO = lettersA 
searchAIO = snd `fmap` runStateT letterAIO  0
