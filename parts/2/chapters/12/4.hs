newtype ZipList a = Z [a] deriving Show


instance Functor ZipList where
  -- fmap :: (a -> b) -> ZipList a -> ZipList b
  fmap g (Z xs) = Z [g x | x <- xs]


instance Applicative ZipList where
  -- pure :: a -> ZipList a
  pure x = Z [x | _ <- [1..]]

  -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
  (Z []) <*> _ = Z []
  _ <*> (Z []) = Z []
  (Z (g:gs)) <*> (Z (x:xs)) = Z ((g x) : bs)
                              where (Z bs) = (Z gs) <*> (Z xs)

