sqroot :: Double -> Double
sqroot n = snd (head (dropWhile far (zip ns (tail ns)))) 
           where
             next a = (a + n / a) / 2
             ns = iterate next 1.0
             far (a, b) = abs (a - b) >= 0.00001
