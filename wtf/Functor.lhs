> {-# LANGUAGE KindSignatures #-}
> module Functor where

> import Prelude hiding (Functor(..),map)

Wszyscy kochamy listy, listy maja prosta budowe, zadnych skomplikowanych niezmiennnikow 
no i znamy je od stuleci. Ponadto do manipulacji nimi posiadamy nieprzebrany arsenal 
roznorakich funkcji zdefiniowanych w Data.List tudziez innych paczkach na hackage'u
jedna z podstawowych funkcji jest znany wszystkich `map`

> map :: (a -> b ) -> [a] -> [b]
> map _	  []	= [] 
> map f (x:xs)	= f x: map f xs 

Milo, potrafimy przejsc strukture listy i pozmieniac jej elementy, podobnie potrafimy napisach 
`mapTree` dla drzew binarnych (tudziez innych)

> data Tree a = Tip | Bin a (Tree a ) (Tree a)
> mapTree :: (a -> b) -> Tree a -> Tree b 
> mapTree _   Tip	   = Tip
> mapTree f (Bin a t1 t2)  = Bin (f a) (mapTree f t1) (mapTree f t2)

Niejest wielkim wysilikiem dostrzezenie podobienstw miedzy tymi funkcjami
a przeciez bardzo lubimy dostrzegac powtarzajace sie schematy, generalizowac je i
zamykach w klasach typow, to do dziela, patrzac na typ widzimy ze to co potrzebujemy
zgeneralizowac to typ kontenera, zatem chcemy miec taka klase:

> class A (f :: * -> *) where
>   map' :: (a -> b) -> f a -> f b

Teraz wypadalo by nadac takiej klasie jakas sensowna nazwe, jak sobie przypomnicie to juz
spotkalismy sie z takim typem tudziez z ta forma abstarkcji podczas ostatniego rozwiaznia 
wariantow polimorficznych, klase ta nazywamy `Functor` co wiaze sie z funktorami z TK.
zatem pelna definicja jest nastepujaca:

> class Functor (f :: * -> * ) where -- nie trzeba podawac jawnie kindow 
>   fmap :: ( a -> b ) -> f a -> f b
>
> instance Functor [] where
>   fmap = map
>
> instance Functor Tree where
>   fmap = mapTree

oczywiscie istnieje wiele innych ciekawych functorow jak Maybe, IO czy ( e -> ) 
mamy tez funktory calkiem trywialne jak 

> data Id a = Id a
> data K1 a b = K1 a
> data K2 a b = K2 b

zeby nie bylo za dobrze funktory musza spelniach pewne prawa, raczej proste 
fmap id = id oraz fmap (f . g) = fmap f . fmap g 

z funktorami zwiazane sa pewne funkcje ktore reprezentuja schemety rekursji po odpwiednich 
strukturach. Najbardziej popularne jest oczywiscie foldowanie strukury 
dla ktorej to operacji matematycy nadali nazwe katamorfizm, tudziez banan z powodu 
nawiasow jakimi sie ja opisuje 
ale o tym moze innym razem wiecej mozna poczytac np tu 
(http://knol.google.com/k/edward-kmett/catamorphisms)

druga podstawowa funkcja to unfoldowanie (unfoldr z Data.List) zwana anamorfizmem 
tudziez szoczewka ww powodow
o zabawach z roznymi przedmiotami poczytac mozna np tu
E. Meijer, M. Fokkinga, R. Paterson, Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire, 5th ACM Conference on Functional Programming Languages and Computer Architecture.
http://research.microsoft.com/~emeijer/Papers/fpca91.pdf



