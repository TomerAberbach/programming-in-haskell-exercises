import Data.Foldable

data Maybe a = Nothing | Just a

instance Foldable Main.Maybe where
  -- fold :: Monoid a => Maybe a -> a
  fold Main.Nothing = mempty
  fold (Main.Just v) = v

  -- foldMap :: Monoid b => (a -> b) -> Maybe a -> b
  foldMap _ Main.Nothing = mempty
  foldMap f (Main.Just v) = f v

  -- foldr :: (a -> b -> b) -> b -> Maybe a -> b
  foldr _ v Main.Nothing = v
  foldr f v1 (Main.Just v2) = f v2 v1

  -- foldl :: (a -> b -> a) -> a -> Maybe b -> a
  foldl _ v Main.Nothing = v
  foldl f v1 (Main.Just v2) = f v1 v2


instance Functor Main.Maybe where
  -- fmap :: (a -> b) -> Maybe a -> Maybe b
  fmap _ Main.Nothing = Main.Nothing
  fmap g (Main.Just v) = Main.Just (g v)


instance Traversable Main.Maybe where
  -- traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
  traverse _ Main.Nothing = pure Main.Nothing
  traverse g (Main.Just v) = fmap Main.Just (g v) 

