listt a = 1:[2,3]

myLength [] = 0
myLength l = 1 + myLength (tail l)

myLengthPattern [] = 0
myLengthPattern (x:xs) = 1 + myLengthPattern xs

mySum [] = 0 
mySum (x:xs) = x + mySum xs 

appendr x [] = [x]
appendr x (y:ys) = y : (appendr x ys)

attach [] l = l
attach (x:xs) l = x:(attach xs l)

reverseptrn [] = []
reverseptrn (x:xs) = (reverseptrn xs) ++ [x]

ascendingptrn [] = True 
ascendingptrn [x] = True
ascendingptrn (x:y:ys) = (x <= y) && ascendingptrn (y:ys)


alternating l = (upDown l) || (downUp l)

upDown [] = True 
upDown [x] = True 
upDown (x:y:ys) = (x < y) && (downUp (y:ys))

downUp [] = True 
downUp [x] = True 
downUp (x:y:ys) = (x>y) && (upDown (y:ys))

myTake n [] = []
myTake n (x:xs) 
 | n == 0 = []
 | n>0 = x: (myTake (n-1) xs)
 | otherwise = []

occurs c "" = False 
occurs c (x:xs)
 | c == x = True 
 | otherwise = occurs c xs 

hasElement x [] = -101
hasElement y (x:xs) 
 | y == x = 1 
 | otherwise = 1 + hasElement y xs 

position c "" = 0
position c (d:ds) 
 | c == d = 0
 | otherwise = 1 + (position c ds)
 
whitespace ' ' = True
whitespace '\t' = True
whitespace '\n' = True
whitespace _ = False 

wscount "" = 0
wscount (c:cs)
 | whitespace c = 1 + wscount cs 
 | otherwise = wscount cs

wordcaux [c] = 0
wordcaux (c:d:ds)
 | (whitespace c) && not(whitespace d) = 1 + wordcaux (d:ds)
 | otherwise = wordcaux (d:ds)
 
wordc s = wordcaux (' ':s)




































































