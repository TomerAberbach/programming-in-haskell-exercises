data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

leaves :: Tree a -> Int
leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node l r) = abs (leaves l - leaves r) <= 1 && balanced l && balanced r


split :: [a] -> ([a], [a])
split xs = (take half xs, drop (full - half) xs) 
           where
             full = length xs
             half = full `div` 2

balance :: [a] -> Tree a
balance [x] = Leaf x
balance xs = Node (balance left) (balance right)
             where
               pair = split xs
               left = fst pair
               right = snd pair

