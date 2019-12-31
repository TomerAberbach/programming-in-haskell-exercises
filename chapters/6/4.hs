euclid :: Int -> Int -> Int
euclid a b | a == b = a
           | otherwise = euclid (larger - smaller) smaller
           where
             smaller = minimum [a, b]
             larger = maximum [a, b]

