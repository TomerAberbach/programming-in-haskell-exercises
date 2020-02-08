import Control.Applicative
import Data.Char
import System.IO

newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) input = p input

item :: Parser Char
item = P (\input -> case input of
                      [] -> []
                      (x:xs) -> [(x, xs)])


instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap g p = P (\input -> case parse p input of
                            [] -> []
                            [(v, out)] -> [(g v, out)])


instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\input -> [(v, input)])

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  pg <*> px = P (\input -> case parse pg input of
                             [] -> []
                             [(g, out)] -> parse (fmap g px) out)


instance Monad Parser where
  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\input -> case parse p input of
                           [] -> []
                           [(v, out)] -> parse (f v) out)


instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\input -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\input -> case parse p input of
                           [] -> parse q input
                           [(v, out)] -> [(v, out)])


sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty


digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char c = sat (== c)

string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: Parser String
ident = do x <- lower
           xs <- many alphanum
           return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
      <|> nat

token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

identifier :: Parser String
identifier = token ident

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol = token . string

nats :: Parser [Int]
nats = do symbol "["
          n <- natural
          ns <- many (do symbol ","
                         natural)
          symbol "]"
          return (n:ns)

binr :: (Int -> Int -> Int) -> Parser String -> Parser Int -> Int -> Parser Int
binr f po pr l = do po
                    r <- pr
                    return (f l r)

add :: Int -> Parser Int
add = binr (+) (symbol "+") expr

sub :: Int -> Parser Int
sub = binr (-) (symbol "-") expr

expr :: Parser Int
expr = do t <- term
          add t <|> sub t <|> return t

mul :: Int -> Parser Int
mul = binr (*) (symbol "*") term

divide :: Int -> Parser Int
divide = binr div (symbol "/") term

term :: Parser Int
term = do p <- power
          mul p <|> divide p <|> return p

expo :: Int -> Parser Int
expo = binr (^) (symbol "^") power

power :: Parser Int
power = do f <- factor
           expo f <|> return f

factor :: Parser Int
factor = do symbol "("
            e <- expr
            symbol ")"
            return e
         <|> integer

-- eval :: String -> Int
-- eval xs = case (parse expr xs) of
--             [(n, [])] -> n
--             [(_, out)] -> error ("Unused input " ++ out)
--             [] -> error "Invalid input"

getCh :: IO Char
getCh = do hSetEcho stdin False
           x <- getChar
           hSetEcho stdin True
           return x

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

writeat :: Pos -> String -> IO ()
writeat p xs = do goto p
                  putStr xs

goto :: Pos -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

box :: [String]
box = ["+---------------+",
       "|               |",
       "+---+---+---+---+",
       "| q | c | d | = |",
       "+---+---+---+---+",
       "| 1 | 2 | 3 | + |",
       "+---+---+---+---+",
       "| 4 | 5 | 6 | - |",
       "+---+---+---+---+",
       "| 7 | 8 | 9 | * |",
       "+---+---+---+---+",
       "| 0 | ( | ) | / |",
       "+---+---+---+---+"]

buttons :: String
buttons = standard ++ extra
          where
            standard = "qcd=123+456-789*0()/^"
            extra = "QCD\ESC\BS\DEL\n"

showbox :: IO ()
showbox = sequence_ [writeat (1, y) b | (y, b) <- zip [1..] box]

display xs = do writeat (3, 2) (replicate 13 ' ')
                writeat (3, 2) (reverse (take 13 (reverse xs)))

beep :: IO ()
beep = putStr "\BEL"

calc :: String -> IO ()
calc xs = do display xs
             c <- getCh
             if elem c buttons then
               process c xs
             else
               do beep
                  calc xs

process :: Char -> String -> IO ()
process c xs | elem c "qQ\ESC" = quit
             | elem c "dD\BS\DEL" = delete xs
             | elem c "=\n" = eval xs
             | elem c "cC" = clear
             | otherwise = press c xs

quit :: IO ()
quit = goto (1, 14)

delete :: String -> IO ()
delete [] = calc []
delete xs = calc (init xs)

index :: String -> String -> Maybe Int
index s1 s2 = index' 0
              where index' n = if (length s2) - n < length s1 then
                                 Nothing
                               else if all (\(c1, c2) -> c1 == c2) (zip s1 (drop n s2)) then
                                 Just n
                               else
                                 index' (n + 1)
eval :: String -> IO ()
eval xs = case parse expr xs of
            [(n, [])] -> calc (show n)
            [(_, cs)] -> do beep
                            case index cs xs of
                              Nothing -> calc xs
                              Just n -> calc $ (take n xs) ++ ['?'] ++ (drop n xs)

clear :: IO ()
clear = calc []

press :: Char -> String -> IO ()
press c xs = calc (xs ++ [c])

run :: IO ()
run = do cls
         showbox
         clear

comment :: Parser ()
comment = do string "--"
             many (sat (/= '\n'))
             char '\n'
             return ()

