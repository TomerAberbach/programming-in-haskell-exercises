luhnDouble :: Int -> Int
luhnDouble n = if doubled > 9 then doubled - 9 else doubled
               where 
                 doubled = n * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = (luhnDouble a + b + luhnDouble c + d) `mod` 10 == 0

