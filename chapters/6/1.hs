-- Applying the factorial function to a negative argument results in infinite recursion.

fac :: Int -> Int
fac 0 = 1
fac n | n < 0 = error "Factorial of a negative integer is undefined."
      | otherwise = n * fac (n - 1)

