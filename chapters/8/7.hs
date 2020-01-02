data Maybe a = Nothing | Just a deriving (Show)

instance Eq a => Eq (Main.Maybe a) where
  Main.Nothing == Main.Nothing = True
  Main.Nothing == _ = False
  _ == Main.Nothing = False
  (Main.Just l) == (Main.Just r) = l == r

-- instance Eq a => Eq [a] where
equal :: Eq a => [a] -> [a] -> Bool
equal as bs = length as == length bs && and (map (uncurry (==)) (zip as bs))

