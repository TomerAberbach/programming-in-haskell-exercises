data Expr = Val Int | Add Expr Expr deriving Show

data Op = PUSH Int | ADD deriving Show

type Code = [Op]

comp :: Expr -> Code
comp (Val n) = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

-- Specification: comp' e c = comp e ++ c
-- 
-- Base case:
--   comp' (Val n) c
--   =    { specification of comp' }
--   comp (Val n) ++ c
--   =    { applying comp }
--   [PUSH n] ++ c
--   =    { applying ++ }
--   PUSH n : c
--
-- Inductive case:
--   comp' (Add l r) c
--   =    { specification of comp' }
--   comp (Add l r) ++ c
--   =    { applying comp }
--   (comp l ++ comp r ++ [ADD]) ++ c
--   =    { associativty of ++ }
--   comp l ++ comp r ++ [ADD] ++ c
--   =    { applying ++ }
--   comp l ++ comp r ++ (ADD:c)
--   =    { induction hypothesis }
--   comp l ++ comp' r (ADD:c)
--   =    { induction hypothesis }
--   comp' l (comp' r (ADD:c))
comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n : c
comp' (Add l r) c = comp' l (comp' r (ADD:c))

expr :: Expr
expr = Add (Add (Val 9) (Val 6)) (Add (Add (Val 2) (Val 4)) (Add (Val 10) (Val 13)))

