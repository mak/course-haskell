{-# OPTIONS -fglasgow-exts #-}

{- Copyright (c) 2005, Amr Sabry, R. Kent Dybvig, Simon L. Peyton Jones -}

-- Control operators for delimited continuations 
-- (implemented using sequences of frames)
-- Transformer

module CC_FrameT (
  CC, Prompt, SubCont, -- abstract types
  runCC,
  newPrompt, pushPrompt,   -- operations on prompts
  letSubCont, pushSubCont, -- operations on subcontinuations
) where

import qualified PromptTR as Prompt
import SeqTR
import Control.Monad.Trans

----------------------------------------------------------------------
-- Types

newtype Frame m r a b = Frame (a -> CC r m b)
type Cont r m a b = Seq (Frame m) r a b

newtype CC r m a = CC (forall ans. Cont r m a ans -> Prompt.P r m ans)
unCC (CC x) = x
type Prompt r a = Prompt.Prompt r a 
type SubCont r m a b = Seq (Frame m) r a b

----------------------------------------------------------------------
-- CC monad 

instance Monad m => Monad (CC r m) where
  return v = CC (\k -> appk k v)
  e1 >>= e2 = CC (\k -> unCC e1 (PushSeg (Frame e2) k))


instance MonadTrans (CC r) where
    lift m = CC (\k -> lift m >>= appk k)

-- The previous equation was derived to guarantee the law
-- that runCC (lift m) === m
-- Indeed, runCC (lift m) reduces to the following
-- Prompt.runP (lift m >>= (\a -> appk (EmptyS id) a))
-- === Prompt.runP (lift m >>= return)

instance MonadIO m => MonadIO (CC r m) where
    liftIO = lift . liftIO

appk :: Monad m => Cont r m a ans -> a -> Prompt.P r m ans
appk (EmptyS f) v = return (f v)
appk (PushP _ k) v = appk k v
appk (PushSeg (Frame f) k) v = unCC (f v) k
appk (PushCO f k) v = appk k (f v)

runCC :: Monad m => (forall r. CC r m a) -> m a
runCC ce = Prompt.runP (unCC ce (EmptyS id))

----------------------------------------------------------------------
-- Exported operations

newPrompt :: Monad m => CC r m (Prompt r a)
newPrompt = CC (\k -> do p <- Prompt.newPrompt; appk k p)

pushPrompt :: Monad m => Prompt r a -> CC r m a -> CC r m a
pushPrompt p e = CC (\k -> unCC e (PushP p k))

letSubCont :: Monad m => 
	      Prompt r b -> (SubCont r m a b -> CC r m b) -> CC r m a
letSubCont p f = 
    CC (\k -> let (subk,k') = splitSeq p k in unCC (f subk) k')

pushSubCont :: Monad m => SubCont r m a b -> CC r m a -> CC r m b
pushSubCont subk e = CC (\k -> unCC e (appendSeq subk k))

----------------------------------------------------------------------
