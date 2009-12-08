> module Applicative where -- albo Control.Applicative 

> import Control.Monad

Nie tak dawano temu Ross Peterson i Conror McBride zapropnowali 
abstrakycjny opis aplikatywnego stylu dla programowania z efektami (effectfull programming)
cos a'la monady ale slabsze, abstrakcje ta nazwyamy funktorami aplikatywnymi i definiujemy 
w nastepujacy sposob

> class Functor f => Applicative f where
>   pure :: a -> f a
>   (<*>) :: f ( a -> b) -> f a -> f b

z nastepujacymi prawami:
1) pure id <*> u  = u
2) pure (.) <*> u <*> v <*> w = u ( v <*> w)
3) pure f <*> pure x = pure (f x)
4) u <*> pure x = pure (\f -> f x) <*> u

ponadto 

fmap f x = pure f <*> x

Jednym z pierwszyszych sukcesow monadycznego programowania byly monadyczne kombinatory 
parsujace, okazauje sie ze tak silna abstrakcja jaka reprezentuja monady jest zbedna aby
moc rozpoznawac podstawowoe gramatyki takie jak CFG albo Attribute grammars 
nad tymi ostatni pracuja/pracowali w Utrecht'cie czego efektem jest system UUAG i EHC 

Troche naklamalem same funktory aplikatywne nie wystarczna, to w koncu tylko abstarkcja
sekwencyjnego wywolywania efektowo, jak zle by to nie brzmialo, ale klamstwo az tak 
wielkie nie bylo i szybko je naprawie ;]

potrzebujemy jeszcze jednej abstarakcji, do funktorow aplikatywnych dodamy wymaganie zeby bylo
monoidami i bedzie git, tak zdefiniowana klasa pozwoli nam wybierac obliczenia wiec nazwiemy 
ja nie inaczej niz Alternative.

> class Applicative f => Alternative f where
>   empty :: f a 
>   (<|> ) :: f a -> f a -> f a

poniewaz chcemy monoidu to musimy spelnia prawa monoidu ;]
empty <|> e = e <|> empty = e 
(a <|> b) <|> c = a <|> (b <|> c)

zeby miec jakas intuicje na liste zadan to przydalo by sie miec jakies przykladowe 
instancje, jak mowilem na poczatku lubimy listy no to listy ;]

> instance Applicative [] where
>   pure = (:[])

z <*> jest ciekawsza zabawa poniewaz implementacja nie jest jednoznaczna
ja podam jedna a druga pozostawimy jako cwieczenie

>   fs <*> xs = [f x | f <- fs, x <- xs]

alternatywa jest tez raczej prosta 

> instance Alternative [] where
>   empty = []
>   (<|>) = (++)

jak mowilem funktory aplikatywne sa slabsze niz monady, co zatem idzie mozna latwo 
wyrazic jedne przez drugie

> newtype WrappedMonad m a = WrapMonad { unWrapMonad :: m a }
> instance Monad m => Applicative (WrappedMonad m ) where
>   pure = WrapMonad . return
>   WrapMonad f <*> WrapMonad x  = WrapMonad $ f `ap` x  -- wymaga import Control.Monad

zapominialem o tym powiedziec wczescniej ale oczywiscie kazda monada jest funktorem

> instance Monad m => Functor (WrappedMonad m) where
>   fmap f = WrapMonad . liftM f . unWrapMonad -- tak jak wyzej

ponadto poniewaz Aplternative to to samo dla Applicative co MonadPlus dla Monad to 

> instance MonadPlus m => Alternative (WrappedMonad m) where
>   empty = WrapMonad mzero
>   WrapMonad a <|> WrapMonad b = WrapMonad $ a `mplus` b

podobne zaleznosci mamy dla strzalek ale o tym szrzej kiedy indziej
