> module Duality where

> import Data.Monoid

Jedna z pieknych rzeczy w teorii kategori jest pojece dualnosci
i to ze kazdy obiekt ma (zazwyczaj) obiet do siebie dualny
dualny obiekt poprzedzamy zwyczajowo przedrostkiem co-
i tak pary <-> co-pary (suma rozlaczna) 
i oczywiscie istnieja CoFunctory i CoMonady CoAlgebry i pewnie sporo innych o ktorych
nie mam pojecia 

obiekty dualne uzyskuje sie nadwyraz latwo, wystarczy pobracac starzalki na diagramie 
reprezentujacym dana kategorie

i tak skoro dla funktora mielismy fmap :: (a -> b) -> f a -> f b
tak dla co-funktora

> class CoFunctor f where
>   cofmap :: (a -> b) -> f b -> f a 

z prawami
1) cofmap id = id
2) cofmap (f . g) = cofmap g . cofmap f

ale to jest jakas bzdura i nigdzie nie widzialem aby ktos cos z tym robil
poza dziwnymi bibliotekami

duzo ciekawszym i majacym praktyczne zastosowanie obiektem jest CoMonada 
comonade mozemy zdefiniowac w dwujnasob tak samo jak monady kto zgadnie jak?

ano tak

> class Functor w => Comonad w where
>   extract :: w a -> a
>   duplicate :: w a -> w ( w a)
>   extend :: (w a -> b) -> w a -> w b 
>   extend f = fmap f . duplicate 
>   duplicate = extend id 

dla wygody zamienilismy miejscami arugmenty w `extend` porzadna funkcja dualna do >>= jest 
nastepujaca

> (=>>) :: Comonad w => w a -> (w a -> b) -> w b
> (=>>) = flip extend


jako przyklad damy cos prostego, wiemy ze (e -> ) jest monada 
otoz jezeli e jest monoide to ( e ->) jest tez comonada

> instance Monoid e => Comonad ((->)e) where
>   extract = ($mempty)
>   duplicate f m = f . mappend m

Polecam sobie przejrzenie http://fmapfixreturn.wordpress.com/2008/07/09/comonads-in-everyday-life/
piekny post o tym ze tak wydawaloby sie egzotyczny obiekt jest obecny w codziennym zyciu, 
w czyms tak nudnym jak programowanie menu dla strony moze sie okazac ciekawym doznaniem 
intelektualnym 

ogolnie rozumujemy tak, monady przydaja sie wtedy kiedy mowimy (dowodzimy ) o funkcjach
ktore generuja jakis efekt, natomiast comonady sluza nam przy funkcjach zaleznych od kontekstu
i sa podstawa paradygmatu zwanego `dataflow programming` 
(The Essence of Dataflow Programming, Tarmo Uustalu1 and Varmo Vene)
o ktorym moze tez kiedys powiem ( jak sam sie naucze ;])

Oczywiscie nie bylo by dobrze gdybysmy dualnosci nie mogli wyrazic w jezyku
ale to pozostawie jako proste zadanie 
