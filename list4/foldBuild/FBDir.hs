{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -O2 #-}
{-#  LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude hiding (map,repeat,sum, take)
foo :: Int -> Int
foo n = sum$ map (uncurry (*)) $  repeat 42 `zip` enumFromTo' 1 n

{-# RULES
 "fold/build" forall k z (g::forall b. (a->b->b) -> b -> b) . 
   foldr k z (build g) = g k z
  #-}

{-# INLINE [0] build #-}
build :: forall a. (forall b.(a -> b -> b) -> b -> b) -> [a]
build g = g (:) []

{-# INLINE [1] enumFromTo' #-}
enumFromTo' a b  = build ( \cons nil -> 
  let asdf x y = if x > y then nil else x `cons` asdf (succ x) y  
  in asdf a b)
{-# INLINE [0] mapFB #-}
mapFB c f x ys = c (f x) ys

{-# INLINE [1] map #-}
map f xs = build ( \cons nil -> foldr (mapFB cons f) nil  xs)
{-# INLINE [1] repeat #-}
repeat x = build (\cons nil -> let xs = x `cons` xs in xs)

{-# INLINE [1] take #-}
take n (x:xs) = build ( \ cons nil -> 
  if n < 0 
     then nil
     else foldr (takeFB cons nil) (const nil) xs n)

{-# INLINE [0] takeFB #-}
takeFB  c n x xs m =  if m <= 1  then x `c` n else x `c` xs (m-1)

{-# INLINE [1] sum #-}
sum = foldr (+) 0 	    

main = print =<< (foo . read  ) `fmap`  getLine -- print $ foo 100 
