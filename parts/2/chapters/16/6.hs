data Tree = Leaf Int | Node Tree Tree deriving Show

nodes :: Tree -> Int
nodes (Leaf _) = 0
nodes (Node l r) = 1 + nodes l + nodes r

leaves :: Tree -> Int
leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r

-- Proof: leaves t = nodes t + 1
--        leaves t - nodes t = 1
-- 
-- Base case:
--   leaves (Leaf n) - nodes (Leaf n)
-- =    { applying leaves }
--   1 - nodes (Leaf n)
-- =    { applying nodes }
--   1 - 0
-- =    { applying subtraction }
--   1
--
-- Inductive case
--   leaves (Node l r) - nodes (Node l r)
-- =    { applying leaves }
--   (leaves l + leaves r) - nodes (Node l r)
-- =    { applying nodes }
--   (leaves l + leaves r) - (1 + nodes l + nodes r)
-- =    { distributive property }
--   (leaves l + leaves r) - 1 - nodes l - nodes r
-- =    { commuative and associative properties }
--   (leaves l - nodes l) + (leaves r - nodes r) - 1
-- =    { induction hypothesis twice }
--   1 + 1 - 1
-- =    { applying subtraction }
--   1

