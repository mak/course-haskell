{-# LANGUAGE TypeOperators
  , MultiParamTypeClasses
  , FlexibleInstances
  , NoMonomorphismRestriction
  , OverlappingInstances
  , TypeSynonymInstances
  , UndecidableInstances
  #-}

import Prelude hiding (const, show, Show, Double)
import qualified Prelude
import qualified Data.Map as M 

data Expr f = In (f (Expr f ))

data (f :+: g) a = Inl (f a ) | Inr (g a)
infixr :+:

instance (Functor f, Functor g) => Functor (f :+: g) where
   fmap f (Inl e) = Inl $ fmap f e
   fmap f (Inr e) = Inr $ fmap f e 

foldExpr f (In t) = f (fmap (foldExpr f) t)

class (Functor sub, Functor sup) => sub :<: sup where
    inj :: sub a -> sup a

instance Functor f => (:<:) f f where
    inj = id
instance (Functor f , Functor g) => (:<:) f (f :+: g) where
    inj = Inl
instance (Functor f , Functor g, Functor h, f :<: g) => (:<:) f (h :+: g) where
    inj = Inr . inj

inject = In . inj

data Const a = Const Int

instance Functor Const where
    fmap _ (Const n) = Const n

const n = inject $ Const n

class Eval f where
    eval' :: f Int -> Int

eval = foldExpr eval'

instance (Eval f1, Eval f2) => Eval (f1 :+: f2) where
    eval' (Inr f) = eval' f
    eval' (Inl f) = eval' f

instance Eval Const where
    eval' (Const n) = n

-- BasePlus

data Plus a = Plus a a

instance Functor Plus where
    fmap f (Plus e1 e2) = Plus (f e1) (f e2)

plus a b = inject $ Plus a b

instance Eval Plus where
    eval' (Plus e1 e2) = e1 + e2


-- BaseNeg

data Neg a = Neg a

instance Functor Neg where
    fmap f (Neg a) = Neg $ f a

neg x = inject $ Neg x

instance Eval Neg where
    eval' (Neg e) = -e

-- Show

class Show f where
    show' :: f String -> String

show = foldExpr show'

instance (Show f1, Show f2) => Show (f1 :+: f2) where
    show' (Inr f) = show' f
    show' (Inl f) = show' f

instance Show Const where
    show' (Const n) = Prelude.show n

instance Show Neg where
    show' (Neg e) = concat ["-(", e, ")"]

instance Show Plus where
    show' (Plus e1 e2) = concat ["(", e1, ")+(", e2, ")"]

data  Mul a = Mul a a

instance Functor Mul where
   fmap f (Mul a b) = Mul (f a) (f b)

mul a b = inject $ Mul a b

instance Eval Mul where
   eval' (Mul a b) = a * b

instance Show Mul where
   show' (Mul e1 e2) = concat ["(", e1, ")*(", e2, ")"]


data Term f a = Pure a | Impure (f (Term f a))

instance Functor f => Functor (Term f) where
   fmap f (Pure a) = Pure $ f  a
   fmap f (Impure e) = Impure (fmap (fmap f ) e)

instance Functor f => Monad (Term f) where
   return a = Pure a 
   (Pure a) >>= f = f a 
   (Impure e) >>= f = Impure $ fmap (>>=f) e 

data Store t = Store (String,Int) t
data Get t = Get String (Int -> t)

instance Functor Store where
   fmap f (Store e t) = Store e (f t)

instance Functor Get where
   fmap f (Get n envFun) = Get n $ f . envFun 

inject' = Impure . inj
get s = inject' (Get s Pure)
store name val = inject' $ Store (name,val) (Pure ())

foldTerm pure _ (Pure a) = pure a
foldTerm pure imp (Impure e) = imp $ fmap (foldTerm pure imp) e 

newtype Mem = Mem (M.Map String Int) deriving Prelude.Show

class Functor f => Run f where
   runAlgebra :: f (Mem -> (a,Mem)) -> Mem -> (a,Mem)

instance (Run f1, Run f2) => Run (f1 :+: f2) where
    runAlgebra  (Inr f) = runAlgebra f
    runAlgebra (Inl f) = runAlgebra f

instance Run Store where
   runAlgebra (Store e t) (Mem m) = t . Mem $ ins e m     
    where ins = uncurry M.insert

instance Run Get where
   runAlgebra (Get n f) (Mem m) = f (get m) (Mem m)
    where get = maybe undefined id . M.lookup n 

test  = do 
  m <- store "v" 6
  v <- get "v"
  let m = plus (const v) (const 3) :: Expr (Plus :+: Const)
  store "v" $ eval m 
  
run = foldTerm (,) runAlgebra 
