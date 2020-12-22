
xor True False = True
xor False True = True 
xor b1 b2 = False

orr True b = True
orr b True = True 
orr b1 b2  = False 

andd True b = b 
andd False b = False

fac 0 = 1 
fac n = n * fac (n-1)

factorial 0 = 1
factorial n 
 | n < 0 = factorial (-n) 
 | n > 1 = n * (factorial (n-1))
 | n > 0 = n * (factorial (n-1))
 
 
plusThree n m p = n + m + p 
 
isCorrect x y z = (x > y) && (y>z)

collecting x y = (x == 'a') && (y == 1)

andpattern True b = b
andpattern False _ = False 

orpattern True _ = True 
orpattern _ True = True 
orpattern _ _ = False

facconditional n
 | n == 0 = 1
 | n > 0 = n * (facconditional (n-1))
 | otherwise = facconditional(-n)
 
largestdiv n = divsearch n (n-1)

divsearch m i 
 | (mod m i) == 0 = i 
 | otherwise = divsearch m (i-1)
 
intlog base 1 = 0
intlog base n 
 | n >= base = 1 + intlog base (div n base) 
 | otherwise = 0

power m 0 = 1 
power m n = m * (power m (n-1))

intreverse n 
 | n < 10 = n 
 | otherwise = (intreverse (div n 10)) + (mod n 10) * (power 10 (intlog 10 n))












































 
 
 
 
 
 
 
 
 
 
 
 
 













