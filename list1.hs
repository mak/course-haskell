module List1 where

fibs ::  IO ()
fibs = fib 0 1 where
  fib x y 
    | x >= limit = return ()
    | even x  = print x >> fib y (x+y)
  fib x y = fib y (x+y)
  limit = 4 * 10 ^ 6
     

facs ::  Integer
facs = sum' $ fac 1 where
  fac n | n /= 100 = n * fac (n+1)
  fac n = 100
  sum' 0 = 0
  sum' n = (n `mod` 10)  + sum' (n `div` 10)


asdf ::  Integer
asdf = aux 0 limit  where
  aux i k | k == 1 = i
	  | even k = aux (i+1) (k `div` 2)
	  | otherwise = aux (i+1) ( 3 * k + 1)
  limit = 10 ^ 6
   



