f1 [] = 3
f1 (a:b:bs) = 4

f 0 = 1
f 1 = 2
f 2 = 3
f _ = -1

con1 f [] = []
con1 f list@(x:xs) = f list x : con1 f xs 

con2 f [] = []
con2 f (x:xs) = f (x:xs) x : con2 f xs

sayMe 1 = "one!"
sayMe 2 = "two!"
sayMe 3 = "three!"
sayMe 4 = "four!"
sayMe 5 = "five!"
sayMe 6 = "six!"

factorial 0 = 1
factorial n = n * factorial (n-1)

head' [] = error "can't call head on an empty list, dummy!"
head' (_:_:_:x:_) = x 

tell [] = "The list is empty"
tell (x:[]) = "The list has one element : " ++ show x 
tell (x:y:[]) = "The list has two elements : " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y 

length' [] = 0
length' (_:vs)= 1+length vs  

sum' [] = 0
sum' (x:xs) = x + sum' xs 

capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x] ++ capital xs

bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty! you should lose weight: " 
    | otherwise     = "ss" 
    where { bmi = weight / height ^ 2;
          skinny = 18.5  ;
          normal = 25.0  ;
          fat = 30.0  ;
	  }
 
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where {
	  (f:_) = firstname;
          (l:_) = lastname ;
	  }

fp [] = 0
fp (' ':xs) = fp xs
fp (x:xs) = 1+ fp xs 















































