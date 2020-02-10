data Tree a = Leaf a | Node (Tree a) (Tree a)

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap g (Leaf x) = Leaf (g x)
  fmap g (Node l r) = Node (fmap g l) (fmap g r)


-- Proof: fmap id = id
--   Base case:
--     - fmap id (Leaf v)
--       =    { applying fmap }
--       Leaf (id v)
--       =    { applying id }
--       Leaf v
--     - id (Leaf v)
--       =    { applying id }
--       Leaf v
--
--  Inductive case:
--    - fmap id (Node l r)
--      =    { applying fmap }
--      Node (fmap id l) (fmap id r)
--      =    { induction hypothesis twice }
--      Node (id l) (id r)
--      =    { applying id twice }
--      Node l r
--    - id (Node l r)
--      =    { applying id }
--      Node l r
--
--
-- Proof: fmap (g . h) = fmap g . fmap h
--   Base case:
--     - fmap (g . h) (Leaf v)
--       =    { applying fmap }
--       Leaf ((g . h) v)
--       =    { applying composition }
--       Leaf (g (h v))
--     - (fmap g . fmap h) (Leaf v)
--       =    { applying composition }
--       fmap g (fmap h (Leaf v))
--       =    { applying fmap }
--       fmap g (Leaf (h v))
--       =    { applying fmap }
--       Leaf (g (h v))
--
--   Inductive case:
--     - fmap (g . h) (Node l r)
--       =    { applying fmap }
--       Node (fmap (g . h) l) (fmap (g . h) r)
--       =    { induction hypothesis twice }
--       Node ((fmap g . fmap h) l) ((fmap g . fmap h) r)
--       =    { applying composition twice }
--       Node (fmap g (fmap h l)) (fmap g (fmap h r))
--     - (fmap g . fmap h) (Node l r)
--       =    { applying composition }
--       fmap g (fmap h (Node l r))
--       =    { applying fmap }
--       fmap g (Node (fmap h l) (fmap h r))
--       =    { applying fmap }
--       Node (fmap g (fmap h l)) (fmap g (fmap h r))

