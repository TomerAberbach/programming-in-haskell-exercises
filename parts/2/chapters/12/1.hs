data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Leaf = Leaf
  fmap f (Node l v r) = Node (fmap f l) (f v) (fmap f r)

tree :: Tree Int
tree = Node (Node (Node Leaf 50 Leaf) 3 Leaf) 10 (Node Leaf 4 Leaf)

