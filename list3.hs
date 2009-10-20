{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}

module List3 where

import Control.Arrow

-- zad0
data Z = Z  
data S a = S a

class Add a b c | a b -> c where
   add :: a -> b -> c

instance Add a Z a where
  add a Z = a

instance Add a b c => Add a (S b) (S c) where
   add a (S b) = S $ add a b

class Sub a b c | a b -> c where
   sub :: a -> b -> c

instance Sub a Z a where
   sub a Z = a

instance Sub a b c => Sub (S a) (S b) c where
  sub (S b) (S a) = sub b a

{->
type family Add a b 
type instance Add a Z = a
type instance Add a (S b) = S (Add a b)

type family Sub a b 
type instance Sub Z Z = Z
type instance Sub (S a ) (S b) = Sub a b

type family Mul a b
type instance  Mul a Z = Z
type instance  Mul a (S b) = Add (Mul a b) a
-} 


data Atom


class IsList  t coll | t -> coll
instance IsList [a]  [()]
instance TypeCast Atom coll =>  IsList t coll


class TypeCast   a b   | a ->b, b->a   where typeCast   :: a ->b
class TypeCast'  t a b | t a ->b, t b ->a where typeCast'  :: t->a->b
class TypeCast'' t a b | t a ->b, t b ->a where typeCast'' :: t->a->b
instance TypeCast'  () a b => TypeCast a b where typeCast x = typeCast' () x
instance TypeCast'' t a b => TypeCast' t a b where typeCast' = typeCast''
instance TypeCast'' () a a where typeCast'' _ x  = x


class MapT a c1  | c1 ->a  where
    mapT :: (a ->a) ->c1 -> c1

instance (IsList c1 coll, MapT' coll a c1  ) 
    => MapT a c1 where
    mapT = mapT' (undefined::coll)

class MapT' coll a c1 | coll c1 ->a where
    mapT' :: coll ->(a ->a) ->c1 ->c1

instance MapT' Atom a a where
    mapT' _ = id

instance MapT a c  => MapT' [()] a [c]  where
    mapT' _ = map . mapT

class Age a n | a -> n where
   age :: a -> n

zipWN :: (ZipWith b  a t, IsFun a b ) => a  -> t
zipWN = uncurry appMany . (isFun &&& repeat ) 
data T
data F 

class IsFun a b | a -> b where
  isFun :: a -> b
  isFun = undefined 
instance TypeCast f T => IsFun (x -> y) f
instance TypeCast f F => IsFun a f

class  ZipWith b s t | b s -> t where 
  appMany :: b -> [s] -> t 

instance ZipWith F f [f] where
  appMany _ =  id 

instance (IsFun t b,ZipWith b t ts )=> 
	 ZipWith T (s -> t) ([s] -> ts) where
  appMany _  fs = appMany (undefined::b) . zipWith ($) fs 

zipWithN f = appMany (isFun f)  $ repeat f 

class Arity a where
   arity :: a -> Int

instance Arity a  where
   arity = const 0 

instance Arity b => Arity ( a -> b )  where 
   arity f = (1+). arity $  f (undefined :: a) 


zipW n = n. repeat 
zero = id
suc n fs = n .  zipWith ($) fs

class FullArr a where
   farity :: a -> Int
instance FullArr a where
   farity = const 0

instance (FullArr b, FullArr a ) => FullArr (a -> b) where
   farity f = (k + ). succ . farity $ f ( undefined :: a)
    where k = farity (undefined :: a)
