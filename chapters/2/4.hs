last1 [x] = x
last1 (x:xs) = last xs

last2 xs = xs !! (length xs - 1)

last3 [x] = x
last3 xs = last3 (tail xs)

