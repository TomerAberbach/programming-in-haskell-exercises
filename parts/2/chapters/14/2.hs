import Data.Monoid

instance Monoid b => Monoid (a -> b) where
  -- mempty :: (a -> b)
  mempty = const mempty

  -- mappend :: (a -> b) -> (a -> b) -> (a -> b)
  f `mappend` g = (\a -> f a `mappend` g a)

