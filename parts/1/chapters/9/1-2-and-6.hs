import Data.List

data Op = Add | Sub | Mul | Div | Exp

instance Show Op where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Exp = "^"

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y > 1 && x `mod` y == 0
valid Exp x y = y > 1

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y
apply Exp x y = x^y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
  show (Val n) = show n
  show (App o l r) = brak l ++ show o ++ brak r
                     where
                       brak (Val n) = show n
                       brak e = "(" ++ show e ++ ")"

values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

count :: Expr -> Int
count (Val _) = 1
count (App _ l r) = count l + count r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l, y <- eval r, valid o x y]

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
              where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices xs = [x | xs' <- subs xs, x <- perms xs']

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls, rs) | (ls, rs) <- split xs]

ops :: [Op]
ops = [Add, Sub, Mul, Div, Exp]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls, rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

type Result = (Expr, Int)

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

results :: [Int] -> [Result]
results [] = []
results [n] = [(Val n, n) | n > 0]
results ns = [res | (ls, rs) <- split ns, lx <- results ls, ry <- results rs, res <- combine' lx ry]

associate :: [(Int, Expr)] -> [(Int, [Expr])]
associate xs = map (\xs' -> (fst (head xs'), sortOn count (map snd xs'))) (groupBy (\x y -> fst x == fst y) (sortOn fst xs))

solutions' :: [Int] -> Int -> (Int, [Expr])
solutions' ns n = head (associate [(abs (m - n), e) | ns' <- choices ns, (e, m) <- results ns'])

removeone :: Eq a => a -> [a] -> [a]
removeone x [] = []
removeone x (y:ys) | x == y = ys
                   | otherwise = y : removeone x ys

isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice (x:xs) [] = False
isChoice (x:xs) ys = elem x ys && isChoice xs (removeone x ys)

main :: IO ()
main = print (solutions' [1, 3, 7, 10, 25, 50] 831)

