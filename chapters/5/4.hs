replicate :: Int -> a -> [a]
replicate n v = [v | _ <- [1..n]]

