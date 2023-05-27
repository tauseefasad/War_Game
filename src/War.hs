module War (main, deal) where

import Data.List

main :: IO ()
main = do
    putStrLn "Hello, world!"

deal :: [Int] -> [Int]
deal shuf = split shuf [] []

--The 'split' function below recursively splits the shuffled deck shuf into two decks s1 and s2 by 
--alternately appending the first two cards of shuf to each of the two decks. 

split :: [Int] -> [Int] -> [Int] -> [Int]
split [] s1 s2 = replaceCards s1 s2
split [n] s1 s2 = replaceCards (n:s1) s2
split (n1:n2:rest) s1 s2 = split rest (n1:s1) (n2:s2)




--The replaceCards function takes two lists of integers as input, which represents the cards held by two players, 
--Returns the updated cards after a round of play
replaceCards :: [Int] -> [Int] -> [Int]
replaceCards s1 s2
  | length s1 == 0 = s2
  | length s2 == 0 = s1
  | s1 == s2 = shuffleCards (s1 ++ s2)
  | head s1 == 1 && head s2 /= 1 = replaceCards (tail s1 ++ [head s1, head s2]) (tail s2)
  | head s1 /= 1 && head s2 == 1 = replaceCards (tail s1) (tail s2 ++ [head s2, head s1])
  | head s1 > head s2 = replaceCards (tail s1 ++ [head s1, head s2]) (tail s2)
  | head s1 < head s2 = replaceCards (tail s1) (tail s2 ++ [head s2, head s1])
  | head s1 == head s2 =
  case (length s1, length s2) of
  (1, _) -> tail s2 ++ shuffleCards [head s1, head s2]
  (_, 1) -> tail s1 ++ shuffleCards [head s1, head s2]
  _      -> playWar (drop 2 s1) (drop 2 s2) (take 2 s1 ++ take 2 s2)



--The function takes as input the current state of the game (s1, s2) 
--The function 'playWar' removes the top two cards from each deck (s1 and s2) and adds them to the accumulated list. 
--It then recursively calls playWar with the updated s1, s2 and list.



playWar :: [Int] -> [Int] -> [Int] -> [Int]
playWar s1 s2 list | ((length s1) == 0) = replaceCards s1 (s2 ++ (shuffleCards list))
                   | ((length s2) == 0) = replaceCards (s1 ++ (shuffleCards list )) s2
                   | ((head s1) == (head s2)) = playWar (tail (tail s1)) (tail (tail s2)) (list ++ [head s1] ++ [head s2] ++ [head (tail s1)] ++ [head (tail s2)])
                   | ((head s1) == 1) = replaceCards ((tail s1) ++ (shuffleCards (list ++ [head s1] ++ [head s2]) )) (tail s2)
                   | ((head s2) == 1) = replaceCards (tail s1) ((tail s2) ++ shuffleCards (list ++ [head s1] ++ [head s2]))
                   | ((head s1) > (head s2)) = replaceCards ((tail s1) ++ shuffleCards (list ++ [head s1] ++ [head s2])) (tail s2)
                   | ((head s1) < (head s2)) = replaceCards (tail s1) ((tail s2) ++ shuffleCards (list ++ [head s1] ++ [head s2]))
                   | otherwise = [] 


--The 'shuffleCards' function takes a list of integers as input  
--And returns a shuffled list of integers as output.
shuffleCards :: [Int] -> [Int]
shuffleCards xs = let (ones, others) = partition (==1) xs
               in ones ++ sortBy (flip compare) others