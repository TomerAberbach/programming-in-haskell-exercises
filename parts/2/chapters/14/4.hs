import Data.Foldable

data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

instance Foldable Tree where
  -- fold :: Monoid a => Tree a -> a
  fold Leaf = mempty
  fold (Node l v r) = fold l `mappend` v `mappend` fold r

  -- foldMap :: Monoid b => (a -> b) -> Tree a -> b
  foldMap _ Leaf = mempty
  foldMap g (Node l v r) = foldMap g l `mappend` g v `mappend` foldMap g r

  -- foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ v Leaf = v
  foldr g v1 (Node l v2 r) = foldr g (g v2 (foldr g v1 r)) l

  -- foldl :: (a -> b -> a) -> a -> Tree b -> a
  foldl _ v Leaf = v
  foldl g v1 (Node l v2 r) = foldl g (g (foldl g v1 l) v2) r


instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap _ Leaf = Leaf
  fmap g (Node l v r) = Node (fmap g l) (g v) (fmap g r)


instance Traversable Tree where
  -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse _ Leaf = pure Leaf
  traverse g (Node l v r) = pure Node <*> traverse g l <*> g v <*> traverse g r

