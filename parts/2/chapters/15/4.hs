fibs :: [Integer]
fibs = 0 : 1 : map (uncurry (+)) (zip fibs (tail fibs))

