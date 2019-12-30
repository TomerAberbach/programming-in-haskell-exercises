init1 [x] = []
init1 (x:xs) = x:(init xs)

init2 xs = take (length xs - 1) xs

