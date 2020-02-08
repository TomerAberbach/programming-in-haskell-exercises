data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving Show

expr :: Expr Char
expr = Add (Add (Add (Var 'x') (Val 4)) (Val 10)) (Add (Var 'y') (Var 'z'))

instance Functor Expr where
  -- fmap :: (a -> b) -> Expr a -> Expr b
  fmap g (Var v) = Var (g v)
  fmap _ (Val n) = Val n
  fmap g (Add l r) = Add (fmap g l) (fmap g r)


instance Applicative Expr where
  -- pure :: a -> Expr a
  pure = Var

  -- (<*>) :: Expr (a -> b) -> Expr a -> Expr b
  (Var f) <*> (Var v) = Var (f v)
  (Var f) <*> (Add l r) = Add (Var f <*> l) (Var f <*> r)
  _ <*> (Val n) = Val n
  (Val n) <*> _ = Val n
  (Add f g) <*> (Add l r) = Add (f <*> l) (g <*> r)


instance Monad Expr where
  -- return :: a -> Expr a
  return = pure

  -- (>>=) :: Expr a -> (a -> Expr b) -> Expr b
  -- Replaces variables with expressions provided by
  -- the function argument
  (Var v) >>= f = f v
  (Val n) >>= _ = Val n
  (Add l r) >>= f = Add (l >>= f) (r >>= f)

