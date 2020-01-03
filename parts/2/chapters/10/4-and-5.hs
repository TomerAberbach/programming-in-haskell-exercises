import Text.Read

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   putChar ' '
                   text <- getLine
                   case readMaybe text :: Maybe Int of
                     Nothing -> invalid
                     Just n -> if n > 0 then return n else invalid
                where invalid = do putStrLn "Invalid natural number!"
                                   getNat prompt

getInt :: IO Int
getInt = do text <- getLine
            case readMaybe text :: Maybe Int of
              Nothing -> do putStrLn "Invalid integer!"
                            getInt
              Just n -> return n

adder :: IO ()
adder = do n <- getNat "How many numbers?"
           ints <- sequence (replicate n getInt)
           putStrLn ("The total is " ++ show (sum ints))

