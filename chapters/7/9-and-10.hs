altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g = map (\(i, x) -> (if even i then f else g) x) . zip [0..]

luhn :: [Int] -> Bool
luhn digits = sum (map (`mod` 9) (double digits)) `mod` 10 == 0
              where
                double = if even (length digits) then
                           altMap (*2) id
                         else
                           altMap id (*2)

