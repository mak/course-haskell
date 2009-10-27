{-# LANGUAGE Rank2Types #-}
module Main where

{-# RULES
 "fold/build -> rule" forall k z (g::forall b. (a->b->b) -> b -> b). 
   foldr k z (build g) = g k z
  #-}

{-# INLINE [0] build #-}
build :: forall a. (forall b. (a -> b -> b ) -> b -> b) -> [a]
build g = g (:) []

{-# RULES
  "mapL -> fused" [~1] forall f xs.
     mapL f xs  = build (\c n -> foldr (mapFB c f) n xs)
  #-}

{-# INLINE [0] mapFB #-}
mapFB c f x xs = f x `c` xs

{-# INLINE [1] mapL#-}
mapL :: (a -> b) -> [a] ->[b]
mapL _ [] = []
mapL f (x:xs) = f x :mapL f xs

{-# INLINE  sumL #-}
sumL :: Num a => [a] -> a
sumL = foldr (+) 0


{-# RULES
  "enumFromToL -> fused" [~1] forall k m.
     enumFromToL k m = build (\c n -> enumFromToLFB c n k m)
  #-}

{-# INLINE [0] enumFromToLFB #-}
enumFromToLFB c n k m = go k where
  {-# INLINE [0] go #-}
  go k = if k > m then n else k `c` go (succ k)

-- {-# INLINE [1] enumFromToL #-}
enumFromToL :: (Enum a,Ord a) => a -> a -> [a]
enumFromToL n m = go n where
  {-#INLINE [1] go #-}
  go k | k > m = []
  go k = k: go (succ k)


{-# INLINE [0] repeatL #-}
repeatL :: a -> [a]
repeatL x = let xs =x : xs  in xs
{-# RULES 
"repeat" [~1] forall x. repeat x = build (\c n -> repeatFB c x)
"repeatFB "  [1]  repeatFB (:) = repeatL
  #-}


{-# INLINE [0] repeatFB #-}
repeatFB c x = let xs  = x `c` xs in xs

zipL (x:xs) (y:ys) = (x,y) : zipL xs ys
zipL _ _ = []
{-# INLINE [0] zipFB #-}
zipFB c x y r = (x,y) `c` r
{-# RULES
"zipL -> fused" [~1] forall xs ys. zipL xs ys = build (\c n -> foldr2 (zipFB c) n xs ys)
 #-}
-- bezwstydnie przepisane z GHC.List...

foldr2 :: (a -> b -> c -> c) -> c -> [a] -> [b] -> c
foldr2 _k z []    _ys    = z
foldr2 _k z _xs   []     = z
foldr2 k z (x:xs) (y:ys) = k x y (foldr2 k z xs ys)
foldr2_left :: (a -> b -> c -> d) -> d -> a -> ([b] -> c) -> [b] -> d
foldr2_left _k  z _x _r []     = z
foldr2_left  k _z  x  r (y:ys) = k x y (r ys)
foldr2_right :: (a -> b -> c -> d) -> d -> b -> ([a] -> c) -> [a] -> d
foldr2_right _k z  _y _r []     = z
foldr2_right  k _z  y  r (x:xs) = k x y (r xs)
-- foldr2 k z xs ys = foldr (foldr2_left k z)  (\_ -> z) xs ys
-- foldr2 k z xs ys = foldr (foldr2_right k z) (\_ -> z) ys xs
{-# RULES
"foldr2/left"   forall k z ys (g::forall b.(a->b->b)->b->b) . 
                  foldr2 k z (build g) ys = g (foldr2_left  k z) (\_ -> z) ys
"foldr2/right"  forall k z xs (g::forall b.(a->b->b)->b->b) . 
                  foldr2 k z xs (build g) = g (foldr2_right k z) (\_ -> z) xs
 #-}

foo :: Int -> Int
foo n = sumL $ mapL (uncurry (*))  $ repeatL 42 `zipL` enumFromToL 1 n
-- bar :: Int -> Int
--bar n = sumL $ enumFromToL 1 n
main = print =<< (foo . read  ) `fmap`  getLine 
