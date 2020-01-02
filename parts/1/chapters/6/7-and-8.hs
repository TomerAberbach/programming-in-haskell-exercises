merge :: Ord a => [a] -> [a] -> [a]
merge [] bs = bs
merge as [] = as
merge (a:as) (b:bs) | a <= b = a : merge as (b:bs)
                    | otherwise = b : merge (a:as) bs

halve :: [a] -> ([a], [a])
halve xs = (take half xs, drop half xs)
           where half = length xs `div` 2

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort (fst halves)) (msort (snd halves))
           where halves = halve xs

