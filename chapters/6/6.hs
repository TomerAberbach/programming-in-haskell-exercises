and :: [Bool] -> Bool
and [] = True
and (b:bs) = b && Main.and bs

concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ Main.concat xss

replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n v = v : Main.replicate (n - 1) v

(!!) :: [a] -> Int -> a
(x:xs) !! 0 = x
xs !! n = (tail xs) Main.!! (n - 1)

elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem v (x:xs) = if v == x then True else Main.elem v xs

