> {-# LANGUAGE RecursiveDo #-}
> module MonadFix where

> import Control.Monad.Fix
> import Control.Monad.ST

Tu bedziemy przelotem tylko bo monady wszak wszyscy doskonale znaja i rozumieja
(jezeli nie to slucham pytan, postaram sie odopowiedziec)

skoro wszyscy wszystko wiedza to zrobimy szybki sprawdzian
Kto potrafi podac poprawna z punktu widzenia TK definicje monady?
a kto kojazy taka funkcje jak `join`?

zatem poprawna definicja monady jest nastepujaca:
monada to trojka skladajaca sie z funktora oraz dwoch transforamcji netralnych
1) η : 1 -> T gdzie 1 to id odpowiedniego funktora
2) μ : T^2 -> T 

w haskellu odpowiadaja temu funkcje
fmap, return i join

tyle na temat ogolnych monad, MonadPlus juz wspomnialem, transformatory juz byly
godna uwagi jest natomiast MonadFix z ktora zapoznamy sie byc moze duzo blizej
(jak tylko przebrne przez pewna prace )
na razie tylko porusze wode w stawie

zatem MonadFix to odpowiednik fix-pointa dla `czystych` funkcji
definicja jest nastepujaca

> class Monad m => MonadFix m where
>   mfix :: ( a -> m a ) -> m a

i prawa

1) mfix (return . h) = return (fix h)
2) mfix (\x -> a >>= \y -> f x y) = a >>= \y -> mfix (\x -> f x y)
3) mfix (fmap h . f) = fmap h (mfix (f . h) dla strict h 
4) mfix (\x -> mfix (\y -> f x y)) = mfix (\x -> f x x)


 
Nie chce sie zabardzo zaglebiac gdyz mozliwe ze bede o tym mowil wiecej w ramch innego spotkania
podam tylko jeden przyklad motywacyjny, pamietacie Cyklisty z Programowania?
troche z tym bylo szarpaniny za pomoca mfix'a mozna to zrobic latwo i przyjemnie

najpierw typ, dodamy referencje aby nie przechodzic listy w kolko 

> newtype Cyclist s a = Cyc (STRef s Bool) (Cyclist s a) a (Cyclist s a)
> data Dir = B | F deriving (Eq,Show)

cons 

> newNode :: Cyclist s a -> a -> Cyclist s a -> ST s (Cyclist s a)
> newNode b e f = newSTRef False >>= \v -> return $ Cyc v b e f

przechodzimy cykliste

> traverse :: Dir -> Cyclist s a -> ST s [a]
> traverse d (Cyc v b e f) = do
>   visited <- readSTRef v
>   if visited then 
>      return [] 
>    else do
>	writeSTRef v True
>	let n = if d == F then f else b
>	es <- traverse d n
>	return $ e : es

zapetlamy liste

> fromList :: [a] -> ST s (Cyclist s a)
> fromList (x:xs) = mdo
>   c <- newNode l x f
>   (l,f) <- fromList' c xs
>   return  c

> formList' :: Cyclist s a -> [a] -> ST s (Cyclist s a,Cyclist s a)
> fromList' p [] = return (p,p)
> fromList' p (x:xs) = mdo 
>   c <- newNode p x f
>   (f,l) <- fromList c xs
>   return (c,l)

i juz, mdo to cukier dla fmix tak jak do to cukier dla >>=
