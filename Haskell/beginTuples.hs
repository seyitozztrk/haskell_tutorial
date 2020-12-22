sumPairs (x,y) = x+y

sumPairList [] = 0
sumPairList ((x,y):zs) = x+y + sumPairList zs

lookUp p [] = -1
lookUp p ((name,marks):ms)
 | (p == name) = marks
 | otherwise = lookUp p ms

 
type Point2D =(Float, Float)

distance (x1,y1) (x2,y2) = sqrt ((x2-x1)*(x2-x1) + (y2-y1)*(y2-y1))

sqr2 x = x*x 

distance2 (x1,y1) (x2,y2) = sqrt (sqr2 (x2-x1) + sqr2 (y2-y1))

distancewhere (x1,y1) (x2,y2) = 
 sqrt (sqrwhere (x2-x1) + sqrwhere (y2-y1))
 where
 sqrwhere x = x*x

distanceAdvance (x1,y1) (x2,y2) = 
 sqrt (sqrAdvance xdiff + sqrAdvance ydiff)
 where
 xdiff = x2 - x1 
 ydiff = y2 - y1 
 sqrAdvance a = a * a

power x 0 = 1 
power x n = x * (power x (n-1))

infinite_list = inflistaux 0 
 where 
 inflistaux n = n:(inflistaux (n+1))

myinit [x] = []
myinit (x:xs) = x:(myinit xs)

f x = x*2
apply x = f x 


sqrList [] = []
sqrList (x:xs) = sqr2 x : (sqrList xs)

sumLength [] = 0
sumLength (x:xs) = length x + (sumLength xs)

sumLength2 l = sum (map length l )

even_only [] = []
even_only (x:xs)
 | is_even x = x:(even_only xs)
 | otherwise = even_only xs 
 where 
  is_even x = (mod x 2) == 0

is_even2 x = (mod x 2) == 0
sqr_even2 l = map sqr2 (filter is_even2 l )

divisors n = [x | x <- [1..n], (mod n x) == 0]

primes n = [x | x <- [1..n], (divisors x == [1,x])]

luppo = [(x,y,z) | x <- [1..100], y <- [(x+1)..100], z <- [(y+1)..100], x*x + y*y == z*z ]

concat2 l = [x | y <- l, x <- y]

head_of_even l = 
 [x | (x:xs) <- l, (mod (length (x:xs)) 2) == 0]

trans = [n*n | n <- [1..7], mod n 2 == 0]

transmap = map f [1..7]
 where 
 f n = [ n*n | mod n 2 == 0]
 
transmapwithifelse = map f [1..7]
 where 
 f n = if (mod n 2 == 0) then [n*n] else []
 
transmapwithconcat =concat (map f [1..7])
 where 
 f n = [ n*n | mod n 2 == 0]

primes2 = sieve [2..20]
 where 
 sieve (x:xs) = x : (sieve [y | y <- xs, mod y x > 0])





































