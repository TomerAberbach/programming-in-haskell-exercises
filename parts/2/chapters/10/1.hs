putStr :: String -> IO ()
putStr cs = sequence_ [putChar c | c <- cs]

