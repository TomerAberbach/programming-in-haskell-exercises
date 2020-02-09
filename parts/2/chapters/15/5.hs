data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

repeat :: a -> Tree a
repeat x = t where t = Node t x t

take :: Int -> Tree a -> Tree a
take 0 _ = Leaf
take _ Leaf = Leaf
take n (Node l v r) = Node (Main.take (n - 1) l) v (Main.take (n - 1) r)

replicate :: Int -> a -> Tree a
replicate n = Main.take n . Main.repeat

