all :: (a -> Bool) -> [a] -> Bool
all p = foldr (&&) True . map p

any :: (a -> Bool) -> [a] -> Bool
any p = foldr (||) False . map p

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile p (x:xs) = if p x then x : Main.takeWhile p xs else []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ [] = []
dropWhile p (x:xs) = if p x then Main.dropWhile p xs else x:xs

