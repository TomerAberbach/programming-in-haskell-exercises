dec2int :: [Int] -> Int
dec2int = foldl (\acc x -> x + 10 * acc) 0

