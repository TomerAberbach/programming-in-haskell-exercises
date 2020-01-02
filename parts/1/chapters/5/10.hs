import Data.Char

let2int :: Char -> Char -> Int
let2int base c = ord c - ord base

int2let :: Char -> Int -> Char
int2let base n = chr (ord base + n)

shift :: Int -> Char -> Char
shift n c | isLower c = int2let 'a' ((let2int 'a' c + n) `mod` 26)
          | isUpper c = int2let 'A' ((let2int 'A' c + n) `mod` 26)
          | otherwise = c

encode :: Int -> String -> String
encode n cs = [shift n c | c <- cs]

table :: [Float]
table = [8.1, 1.5, 2.8, 4.2, 12.7, 2.2, 2.0, 6.1, 7.0,
         0.2, 0.8, 4.0, 2.4, 6.7, 7.5, 1.9, 0.1, 6.0,
         6.3, 9.0, 2.8, 1.0, 2.4, 0.2, 2.0, 0.1]

percent :: Int -> Int -> Float
percent n m = (fromIntegral n / fromIntegral m) * 100

letters :: String -> Int
letters cs = length [c | c <- cs, isLower c || isUpper c]

count :: Char -> String -> Int
count x xs = length [x' | x' <- xs, toLower x == toLower x']

freqs :: String -> [Float]
freqs xs = [percent (count x xs) n | x <- ['a'..'z']]
           where n = letters xs

chisqr :: [Float] -> [Float] -> Float
chisqr os es = sum [((o - e)^2) / e | (o, e) <- zip os es]

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']

crack :: String -> String
crack xs = encode (-factor) xs
           where
             factor = head (positions (minimum chitab) chitab)
             chitab = [chisqr (rotate n table') table | n <- [0..25]]
             table' = freqs xs

