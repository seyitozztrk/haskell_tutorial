condition1 x | x<0 = -1 
	     | x>0 = 1
	     | x==0 = 0

condition2 y = if y>0 then 1 else if y<0 then -1 else 0 

printN' n | n==0 = "" | otherwise = printN' (n-1) ++ show n ++ "\n"

printN n  = putStr (printN' n)


elemPos (a,b,i) = if i > 10000000000 then -1 else if head(b) == a then i else elemPos(a,tail(b),i+1);

f x | x < 0 = g a + g b | a > b = g b | otherwise = c +10 where {a = x * 5;  b = a*2+x;  g t = log t + a;         c = a*3; log b = b*2;}

fx 0 = 1
fx 1 = 2
fx 2 = 3
fx _ = -1