cons8 [] = 0
cons8 [1,2,3] = 6
cons8 [5,6] = 11
{-
	bu kısım yorum satırıdır!!
-}
main = do 
  putStrLn "Please enter your name "
  name <- getLine {-kullanıcıdan input istiyoruz.-}
  putStrLn ("Hello, " ++ name ++ " , how are you?")

{-
doGuessing num = do 
   putStrLn "Enter your guess :"
   guess <- getLine 
   if (read guess) < num 
     then do putStrLn "Too low!"
        doGuessing num 
     else if (read guess) > num
        then do putStrLn "Too high!"
          doGuessing num 
       else putStrLn "You Win!"
not working-}

doubleList [] = []
doubleList (n:ns) = (2*n) : doubleList ns

multiplyList 0 _ =  []
multiplyList m (n:ns) = (n) : multiplyList (m-1) ns
