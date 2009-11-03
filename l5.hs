{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module L4 where


import HList 
import HList2
import HBool
import HNat 

class App f a b | f a -> b where
   app :: f -> a -> b

class HList l => HNull l b | l -> b  where
   hnil :: l -> b
   hnil _ = undefined :: b 

instance HNull HNil HTrue
instance HList l => HNull (HCons e l ) HFalse

class HList l => HFoldl f l b' b | f l b' -> b where
   hfoldl :: f -> b'-> l -> b
instance HFoldl f HNil b b  where
  hfoldl _ b _  = b

instance (HFoldl f l b' b,App f (e,b'') b') => HFoldl f (HCons e l) b'' b where
   hfoldl f b (HCons e l) = hfoldl f (app f (e,b)) l

data Rev
instance HList l => App Rev (e,l) (HCons e l) where
   app _ (e,l) = HCons e l

rev = hfoldl (undefined:: Rev) HNil

data Label n = Label n String

firstLabel l = Label Zero l
nextLabel (Label n _) l = Label (Succ n) l


-- zip/unzip na listach heterogenicznych
class HZip l1 l2 l | l1 l2 -> l, l -> l1 l2 where
    hZip :: l1 -> l2 -> l
    hUnzip :: l -> (l1,l2)

instance HZip HNil HNil HNil where
    hZip _ _ = HNil
    hUnzip _ = (HNil, HNil)

instance HZip l1 l2 l => HZip (HCons e1 l1) (HCons e2 l2) (HCons (e1,e2) l) where
    hZip (HCons e1 l1) (HCons e2 l2) = HCons (e1,e2) $ hZip l1 l2
    hUnzip (HCons (e1,e2) l) = let (l1,l2) = hUnzip l
                               in (HCons e1 l1, HCons e2 l2)

testZip = hZip (Zero .*. Succ Zero .*. Succ (Succ Zero) .*. HNil) ('a' .*. "ala" .*. () .*. HNil)

data Record a = Record a -- nie exportujemy konstruktora Record
                         -- bo bedziemy uzywac wersji ktora ogranicza
                         -- postac listy w srodku

mkRecord :: (HZip labels values l, AllDifferent labels) => l -> Record l
mkRecord = Record

(.=.) = (,)
infix 4 .=.


-- nasz rozszerzacz rekordow, podobny do HConsa, oleg nawet ma osobna klase na wszystkie rozszerzacze
-- zeby uzywac tego samego .*. , my zrobimy inny extender
-- rozszerzac mozemy tylko o nowe pola

(.**.) :: ( HZip oldLabels oldValues r
          , HOccursNot label oldLabels
          , AllDifferent oldLabels)
          => (label, value) -> Record r -> Record (HCons (label,value) r)
(label, value) .**. Record r = Record $ HCons (label,value) r
infixr 2 .**.
data LabelNotFound a 
data TypeMismatch a b 

class HLookupByLabel record label r | record label -> r where
    hLookupByLabel :: record -> label -> r

instance Fail (LabelNotFound label) => HLookupByLabel (Record HNil) label a where
    hLookupByLabel = undefined

instance HLookupByLabel (Record (HCons (l,v) rest)) l v where
    hLookupByLabel (Record (HCons (l,v) _)) _ = v

instance HLookupByLabel (Record rest) l' r => HLookupByLabel (Record (HCons (l,v) rest)) l' r where
    hLookupByLabel (Record (HCons _ r)) l = hLookupByLabel (Record r) l

(.!.) = hLookupByLabel

class HUpdate rec new where
  hUpdate  :: rec -> new -> rec 

instance U r n=> HUpdate (Record r) n where
   hUpdate (Record r)= Record  . hU r 

class U  r n where
   hU :: r -> n -> r
   hU = undefined 


instance U rest e  => U{-'-}(HCons e' rest) e  where
   hU{-'-}(HCons e rest) n = HCons e (hU rest n )

instance U (HCons e  rest) e  where
   hU (HCons _ rest) n = HCons n rest

instance Fail (LabelNotFound l) => U  HNil (l,val)

{-
-- nie udane ladne bledy, nie tylko brak rekordu ale takze niezgodnosc typow 
class U' r n where
   hU' :: r -> n ->  r 
instance U' r n => U r n where
   hU = hU'

instance UFail r n => U' r n where
   hU' = undefined 

class UFail r n 
instance Fail (LabelNotFound l) => UFail  HNil (l,val)
instance Fail (TypeMismatch e e') => UFail (HCons (l,e) r) (l,e')
-}


infixr 2 .->.
(.->.) ::  HUpdate rec new => rec -> new -> rec
(.->.) = hUpdate

ala = name .=. "ala"
 .**. age  .=. (12 :: Int)
 .**. emptyRecord

name = firstLabel "name"
age = nextLabel name "age"
sleepy = nextLabel age "sleepy"
emptyRecord = mkRecord HNil


instance PR r => Show (Record r) where
   show (Record r) = "Record{" ++ pr r ++ "}"

class PR l where
   pr :: l -> String 


instance Show e => PR (HCons (Label l,e) HNil) where
   pr (HCons (Label _ n,val) HNil) = n ++ " = " ++ show val 



instance (Show e,PR r) => PR (HCons (Label l,e) r) where
   pr (HCons (Label _ n,val) r) = n ++ " = " ++ show val ++", "  ++ pr r


