or1 :: Bool -> Bool -> Bool
or1 False False = False
or1 False True = True
or1 True False = True
or1 True True = True

or2 :: Bool -> Bool -> Bool
or2 False False = False
or2 _ _ = True

or3 :: Bool -> Bool -> Bool
or3 True _ = True
or3 False b = b

or4 :: Bool -> Bool -> Bool
or4 b c | b == c = b
        | otherwise = True

