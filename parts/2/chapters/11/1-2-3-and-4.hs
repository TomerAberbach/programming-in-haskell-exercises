import Data.Char
import Data.List
import Data.Ord
import System.IO
import System.Random

size :: Int
size = 3

lineLength :: Int
lineLength = 3

data Player = O | B | X
              deriving (Eq, Ord, Show)

type Grid = [[Player]]

next :: Player -> Player
next O = X
next X = O

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = all (/= B) . concat

turn :: Grid -> Player
turn g = if os <= xs then O else X
         where
           os = length (filter (== O) ps)
           xs = length (filter (== X) ps)
           ps = concat g

diags :: Grid -> [[Player]]
diags g = map (\row -> map (\col -> g !! (row + col) !! col) [0..(size - row - 1)]) [0..(size - 1)]

wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ diags')
           where
             line ps = if length ps < lineLength then False else or [all (== p) (take lineLength (drop i ps)) | i <- [0..(length ps - lineLength)]]
             rows = g
             cols = transpose g
             diags' = diags g ++ diags (map reverse g)

won :: Grid -> Bool
won g = wins O g || wins X g

showPlayer :: Player -> [String]
showPlayer O = ["   ", " O ", "   "]
showPlayer B = ["   ", "   ", "   "]
showPlayer X = ["   ", " X ", "   "]

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
          where
            beside = foldr1 (zipWith (++))
            bar = replicate 3 "|"

valid :: Grid -> Int -> Bool
valid g i = 0 <= i && i < size^2 && concat g !! i == B

chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

move :: Grid -> Int -> Player -> [Grid]
move g i p = if valid g i then [chop size (xs ++ [p] ++ ys)] else []
             where (xs, B:ys) = splitAt i (concat g)

putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
          where bar = [replicate ((size * 4) - 1) '-']

getNat :: String -> IO Int
getNat prompt = do putStr prompt
                   cs <- getLine
                   if cs /= [] && all isDigit cs then
                     return (read cs)
                   else
                     do putStrLn "ERROR: Invalid number"
                        getNat prompt

tictactoe :: IO ()
tictactoe = run empty O

cls :: IO ()
cls = putStr "\ESC[2]"

goto :: (Int, Int) -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

run :: Grid -> Player -> IO ()
run g p = do cls
             goto (1, 1)
             putGrid g
             run' g p

run' :: Grid -> Player -> IO ()
run' g p | wins O g = putStrLn "Player O wins!\n"
         | wins X g = putStrLn "Player X wins!\n"
         | full g = putStrLn "It's a draw!\n"
         | otherwise = do i <- getNat (prompt p)
                          case move g i p of
                            [] -> do putStrLn "ERROR: Invalid move"
                                     run' g p
                            [g'] -> run g' (Main.next p)
                       where prompt p = "Player " ++ show p ++ ", enter your move: "

data Tree a = Node a [Tree a]
              deriving Show

treeValue :: Tree a -> a
treeValue (Node v _) = v

children :: Tree a -> [Tree a]
children (Node _ ts) = ts

child :: Eq a => a -> Tree a -> Tree a
child x = head . filter ((== x) . treeValue) . children

count :: Tree a -> Int
count (Node _ ts) = 1 + sum (map count ts)

deepest :: Tree a -> Int
deepest (Node _ []) = 0
deepest (Node _ ts) = 1 + maximum (map deepest ts)

gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g [gametree g' (Main.next p) | g' <- moves g p]

moves :: Grid -> Player -> [Grid]
moves g p | won g || full g = []
          | otherwise = concat [move g i p | i <- [0..((size^2) - 1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n - 1) t | t <- ts]

depth :: Int
depth = 9

minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g []) | wins O g = Node (g, O) []
                    | wins X g = Node (g, X) []
                    | otherwise = Node (g, B) []
minimax (Node g ts) | turn g == O = Node (g, minimum ps) ts'
                    | turn g == X = Node (g, maximum ps) ts'
                                    where
                                      ts' = map minimax ts
                                      ps = [p | Node (_, p) _ <- ts']

bestmove :: Grid -> Player -> IO Grid
bestmove g p = do i <- randomRIO (0, length bestmoves - 1)
                  return (bestmoves !! i)
               where
                 bestmoves = [g' | Node (g', p') _ <- ts, p' == best]
                 tree = prune depth (gametree g p)
                 Node (_, best) ts = minimax tree

bestmove' :: Grid -> Tree Grid -> Player -> Grid
bestmove' g tree p = (fst . treeValue) (minimumBy (comparing deepest) bestmoves)
                where
                  bestmoves = [t | t <- ts, snd (treeValue t) == best]
                  Node (_, best) ts = minimax tree

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          putStr "Want to start first? (y/n) "
          answer <- getLine
          p <- return (if answer == "y" then O else X)
          tree <- return (gametree g p)
          play g tree O p
          where g = empty

play :: Grid -> Tree Grid -> Player -> Player -> IO ()
play g tree u p = do cls
                     goto (1, 1)
                     putGrid g
                     play' g tree u p

play' :: Grid -> Tree Grid -> Player -> Player -> IO ()
play' g tree u p | wins O g = putStrLn "Player O wins!\n"
                 | wins X g = putStrLn "Player X wins!\n"
                 | full g = putStrLn "It's a draw!\n"
                 | p == u = do i <- getNat (prompt p)
                               case move g i p of
                                 [] -> do putStrLn "ERROR: Invalid move"
                                          play' g tree u p
                                 [g'] -> play g' (child g' tree) u (Main.next p)
                 | otherwise = do putStr ("Player " ++ show p ++ " is thinking...")
                                  m <- return (bestmove' g tree p)
                                  play m (child m tree) u (Main.next p)
            where prompt p = "Player " ++ show p ++ ", enter your move: "

