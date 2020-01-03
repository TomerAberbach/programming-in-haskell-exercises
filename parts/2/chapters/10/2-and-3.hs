import Data.Char

next :: Int -> Int
next 1 = 2
next 2 = 1

type Board = [Int]

initial :: Board
initial = [6, 5, 4, 3, 2, 1]

finished :: Board -> Bool
finished = all (== 0)

valid :: Board -> Int -> Int -> Bool
valid board row num = board !! (row - 1) >= num

move :: Board -> Int -> Int -> Board
move board row num = [update r n | (r, n) <- zip [1..] board]
                     where update r n = if r == row then n - num else n

putRow :: Int -> Int -> IO ()
putRow row num = do putStr (show row)
                    putStr ": "
                    putStrLn (concat (replicate num "* "))

putBoard' :: Board -> IO ()
putBoard' board = put 1 board
                  where
                    put n [] = return ()
                    put n (r:rs) = do putRow n r
                                      put (n + 1) rs

putBoard :: Board -> IO ()
putBoard board = sequence_ [putRow n row | (n, row) <- zip [1..] board]

getDigit :: String -> IO Int
getDigit prompt = do putStr prompt
                     c <- getChar
                     putChar '\n'
                     if isDigit c then
                        return (digitToInt c)
                     else
                        do putStrLn "ERROR: Invalid digit"
                           getDigit prompt

play :: Board -> Int -> IO ()
play board player = do putChar '\n'
                       putBoard board
                       if finished board then
                          do putChar '\n'
                             putStr "Player "
                             putStr (show (next player))
                             putStrLn " wins!!"
                       else
                          do putChar '\n'
                             putStr "Player "
                             putStrLn (show player)
                             row <- getDigit "Enter a row number: "
                             num <- getDigit "Stars to remove: "
                             if valid board row num then
                                play (move board row num) (next player)
                             else
                                do putChar '\n'
                                   putStrLn "ERROR: Invalid move"
                                   play board player

nim :: IO ()
nim = play initial 1

