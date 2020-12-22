infinite_list = inflistaux 0 
 where 
 inflistaux n = n : (inflistaux (n+1) )
 

sqrlist [] = [] 
sqrlist (x:xs) = sqr x : (sqrlist xs )
 where 
 sqr x = x*x

sumLength [] = 0
sumLength (x:xs) = length x + (sumLength xs )

even_only [] = [] 
even_only (x:xs) 
 | is_even x = x : (even_only xs) 
 | otherwise = even_only xs 
 where
  is_even x = (mod x 2) == 0
  
filter p [] = []
filter p (x:xs) 
 | (mod x p ) == 0 = x : (filter p xs)
 | otherwise = filter p xs 
 
  