sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

