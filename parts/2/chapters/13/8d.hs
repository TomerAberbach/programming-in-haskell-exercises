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

expr :: Parser Int
expr = do n <- natural
          ns <- many (do symbol "-"
                         natural)
          return $ foldl (-) n ns

eval :: String -> Int
eval xs = case (parse expr xs) of
            [(n, [])] -> n
            [(_, out)] -> error ("Unused input " ++ out)
            [] -> error "Invalid input"

