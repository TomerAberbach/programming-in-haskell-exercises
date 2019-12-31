import Data.Char

type Bit = Int

bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2 * y) 0

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

parity :: [Bit] -> Bit
parity bits = sum bits `mod` 2

encode :: String -> [Bit]
encode = concat . map ((\bits -> parity bits : bits) . make8 . int2bin . ord)

chop9 :: [Bit] -> [[Bit]]
chop9 [] = []
chop9 bits = take 9 bits : chop9 (drop 9 bits)

verify :: [Bit] -> [Bit]
verify (bit:bits) = if parity bits == bit then bits else error "Parity mismatch!"

decode :: [Bit] -> String
decode = map (chr . bin2int) . map verify . chop9

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = tail

