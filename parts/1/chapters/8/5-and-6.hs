data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f _ (Val n) = f n
folde f g (Add l r) = g (folde f g l) (folde f g r)

eval :: Expr -> Int
eval e = folde id (+) e

size :: Expr -> Int
size e = folde (const 1) (+) e

