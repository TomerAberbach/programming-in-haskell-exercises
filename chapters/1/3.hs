product [] = 1
product (n:ns) = n * Main.product ns

{-|
 - product [2, 3, 4]
 - 2 * product [3, 4]
 - 2 * (3 * product [4])
 - 2 * (3 * (4 * product []))
 - 2 * (3 * (4 * 1))
 - 2 * (3 * 4)
 - 2 * 12
 - 24
 -}

