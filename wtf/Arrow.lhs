> {-# LANGUAGE TypeOperators #-}
> module Arrow where

> import Prelude hiding (id,(.))
> import qualified Prelude

> infixr 3 ***
> infixr 3 &&&
> infixr 2 +++
> infixr 2 |||
> infixr 1 >>>, <<<

Ciekawe te wszystkie slajdy mialy byc wstepem do tego, ktory zamierzalem rozwinac w najblizszej
przyszlosci a okazalo sie ze po drodze dotarlem do sporej ilosci ciekawych pracy ktore 
uswiadomily mi jak plytkie bylo/jest moje zrozumienie tematu 

Ale do rzeczy, najpierw stworzymy abstrakcje opisujaca kategorie 
jak ucza nas zrodla poprzez kategorie rozumiemy zbior obietkow i strzalek pomiedzy nimi
z okreslonymi dzialaniami id, i . zatem

> class Category (~>) where
>   id :: a ~> a 
>   (.) :: (b ~> c) -> (a ~> b) -> (a ~> c)

ponadto (id,(.)) musza tworzyc monoid.

troche tu oszukujemy oczywiscie bo nie mozemy uchwycic dowolnej kategorii tutaj tylko te 
ktorych obiekty sa obiektami w Hask 
ale pomimo to mozemy zdefiniowac jedna trywialna i jedna ciekawa ( i sporo innych )
podstawawa kategoria jaka nas interesuje to Hask z typami jako obietky i funkcjami jako
morfizmy raczej prosta spawa

> instance Category (->) where
>   id = Prelude.id
>   (.) = (Prelude..)

inna ciekawa kategoria swoja nazw bierze od matematyka Heinricha Kleisli 
wyglada ona nastepujaco

> newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b } 

zajome czyz nie?

instance Monad m => Category (Kleisli m ) where (cwiczenie )

dla wygody definiujemy kompozycje w dwie strony

> (<<<) :: Category (~>) =>  b ~> c -> a ~> b -> a ~> c
> (<<<) = (.)

> (>>>) :: Category (~>) => a ~> b -> b ~> c -> a ~> c
> (>>>) = flip (<<<)



Jak zdazylismy zauwazyc to cale to zamieszanie jest o szukanie odpowiedniej abstrakcji
dla programowania z efektami przy czym Arrows jest najbardziej generalna forma jak znamy
mozemy ulozyc sobie jakis taki pozadek:
Functor < Applicative < Monad < Arrows i zupelnie z boku Comonad 
chociaz Comonad < Arrow dla pewnych kategori (chociazby takich jak CoKleisli ;])

zatem jak wyglada ten graal? dosc niepozornie

> class Category (~>) => Arrow (~>) where
>   arr :: (a -> b) -> (a ~> b)

znajome?

>   first :: ( a ~> c) -> ((a,b) ~> (c,b)) 
>   second :: ( b ~> c) -> ((a,b)  ~> (a,c))

a skoro juz pracujemy na produktach to wprowadzmy pare funkcji z TK

>   (***) :: ( b ~> c) -> (b' ~> c') -> ((b,b') ~> (c,c'))
>   (&&&) :: ( b ~> c) -> (b ~> c') -> ( b ~> (c,c'))

produkty wprowadzamy po to zeby moc sekwencjonowac obliczenia 
intuicje sa oczywiste, first (second) zmienia jadna z przekazywanych wartosci druga przekazujac
w nie znienionym stanie

a poniewaz (***) to produkt f g zatem 

>   first f = f *** id
>   second g = id *** g

ponadto 

>   f &&& g = arr (\b -> (b,b)) >>> f *** g

tudziez 

  f *** g = first f >>> second g

i jakze by inaczej troche prawa

1) arr id = id
2) arr (h . g) = arr g >>> arr h
3) first (arr g) = arr (g *** id)
4) first (g >>> h) = first g >>> first h
5) first g >>> arr (id *** h) = arrr (id *** h) >>> first g
6) first g >>> arr fst = arr fst >>> g
7) first (first g) >>> arr assoc = arr assoc >> first g

assoc ((x,y),x) = (x,(y,z))

na razie nasze strzalki nie sa zbyt elestyczne, nie mozemy wybieraz alternatywnych sciezek
obliczen bazujac na kontekscie obliczen, trzeba to naprawic, w tym celu dodamy kolejna klase
roznica jest taka ze potrzebujemy wyboru a do tego sie swietnie nadaja co-produkty 

> class Arrow (~>) => ArrowChoice (~>) where
>   left :: (b ~> c) -> (Either b d ~> Either c d)
>   right :: (b ~> c) -> (Either d b ~> Either d c)
>   (+++) :: (b ~> c) -> (b'~> c') -> (Either b b' ~> Either c c')
>   (|||) :: (b ~> d) -> (c ~> d) -> (Either b c ~> d)


widzimy tu piekna dualnosc pomiedzy parami a co-parami
istnieje jeszcze inne klasy dla strzalek jak chociazby monoidalna ArrowPlus, 
odpowiednik MonadFix, ArrowLoop i inne ale o tym moze kiedy indziej wszystkie one oczywiscie 
spelniaja odpowiednie prawa ;]
