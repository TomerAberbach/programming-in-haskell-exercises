-- To avoid conflict with GHC.Base
newtype Func a b = F (a -> b)

get :: Func a b -> (a -> b)
get (F f) = f

instance Functor (Func a) where
  -- fmap :: (b -> c) -> Func a b -> Func a c
  fmap f (F g) = F (f . g)


instance Applicative (Func a) where
  -- pure :: b -> (Func a b)
  pure v = F (const v)

  -- <*> :: Func a (b -> c) -> Func a b -> Func a c
  (F f) <*> (F g) = F (\x -> (f x) (g x))


instance Monad (Func a) where
  -- return :: b -> Func a b
  return = pure

  -- >>= :: Func a b -> (b -> Func a c) -> Func a c
  (F f) >>= g = F (\a -> ((get . g . f) a) a)

