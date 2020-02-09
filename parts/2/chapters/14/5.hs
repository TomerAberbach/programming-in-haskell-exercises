import Data.Foldable

filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF p t = foldMap (\v -> if p v then [v] else []) t

