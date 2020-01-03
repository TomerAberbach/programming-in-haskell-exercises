import System.IO

getCh :: IO Char
getCh = do hSetEcho stdin False
           c <- getChar
           hSetEcho stdin True
           return c

readLine :: IO String
readLine = do c <- getCh
              case c of
                '\n' -> do putChar '\n'
                           return []
                '\DEL' -> do putStr "\b \b"
                             cs <- readLine
                             return ('\DEL':cs)
                otherwise -> do putChar c
                                cs <- readLine
                                return (case cs of
                                  [] -> [c]
                                  ('\DEL':cs') -> cs'
                                  otherwise -> c:cs)

