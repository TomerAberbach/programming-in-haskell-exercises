data Expr = Val Int
          | Add Expr Expr
          | Throw
          | Catch Expr Expr deriving Show

eval :: Expr -> Maybe Int
eval (Val n)     = Just n
eval (Add x y)   = do n <- eval x
                      m <- eval y
                      return (n + m)
eval Throw       = Nothing
eval (Catch x h) = case eval x of
                     Just n -> Just n
                     Nothing -> eval h

-- Goal:
--   data Code
--   comp :: Expr -> Code
--   comp' :: Expr -> Code -> Code
--   exec :: Code -> Stack -> Stack
-- 
-- Specification:
--   exec (comp e) s = eval e : s
--   exec (comp' e c) s = exec c (eval e : s)
--
-- Define: comp'
--   Case e = Val n:
--     exec (comp' (Val n) c) s
--     =    { specification of comp' }
--     exec c (eval (Val n) : s)
--     =    { applying eval }
--     exec c (Just n : s)
--     =    { c, n unbound in exec c' s:
--            - define:
--              - PUSH :: Int -> Code -> Code
--              - exec (PUSH n c) s = exec c (Just n : s)
--            - unapplying exec }
--     exec (PUSH n c) s
--     =    { c' = PUSH n c in exec c' s = exec (PUSH n c) s:
--            - define comp' (Val n) c = PUSH n c }
--   
--   Case e = Throw:
--     exec (comp' Throw c) s
--     =    { specification of comp' }
--     exec c (eval Throw : s)
--     =    { applying eval }
--     exec c (Nothing : s)
--     =    { c unbound in exec c' s:
--            - define:
--              - THROW :: Code -> Code
--              - exec (THROW c) s = exec c (Nothing : s)
--            - unapplying exec }
--     exec (THROW c) s
--     =    { c' = THROW c in exec c' s = exec (THROW c) s:
--            - define comp' Throw c = THROW c }
--
--   Case e = Add x y:
--     exec (comp' (Add x y) c) s
--     =    { specification of comp' }
--     exec c (eval (Add x y) : s)
--     =    { applying eval }
--     exec c ((do n <- eval x
--                 m <- eval y
--                 return (n + m)) : s)
--     =    { x, y, c unbound in exec c' s:
--            - define:
--              - ADD :: Code -> Code
--              - exec (ADD c) (my : mx : s) = exec c (sum : s)
--                                             where sum = do n <- mx
--                                                            m <- my
--                                                            return (n + m)
--            - unapplying exec }
--     exec (ADD c) (eval y : eval x : s)
--     =    { induction hypothesis for y }
--     exec (comp' y (ADD c)) (eval x : s)
--     =    { induction hypothesis for x }
--     exec (comp' x (comp' y (ADD c))) s
--     =    { c' = comp' x (comp' y (ADD c)) in exec c' s = exec (comp' x (comp' y (ADD c))) s:
--            - define comp' (Add x y) c = comp' x (comp' y (ADD c))
--
--   Case e = Catch x h:
--     exec (comp' (Catch x h) c) s
--     =    { specification of comp' }
--     exec c (eval (Catch x h) : s)
--     =    { applying eval }
--     exec c ((case eval x of
--                Just n -> Just n
--                Nothing -> eval h) : s)
--     =    { x, y, c unbound in exec' c s:
--            - define:
--              - CATCH :: Code -> Code
--              - exec (CATCH c) (h : x : s) = exec c (caught : s)
--                                             where caught = case x of
--                                                              Just n -> Just n
--                                                              Nothing -> h
--            - unapplying exec }
--     exec (CATCH c) (eval h : eval x : s)
--     =    { induction hypothesis for h }
--     exec (comp' h (CATCH c)) (eval x : s)
--     =    { induction hypothesis for x }
--     exec (comp' x (comp' h (CATCH c))) s
--     =    { c' = comp' x (comp' h (CATCH c)) in exec c' s = exec (comp' x (comp' h (CATCH c))) s:
--            - define comp' (Catch x h) c = comp' x (comp' h (CATCH c))
--
-- Define: comp
--   exec (comp e) s
--   =    { specification of comp }
--   eval e : s
--   =    { define:
--          - HALT :: CODE
--          - exec HALT s = s
--          unapplying exec }
--   exec HALT (eval e : s)
--   =     { specification of comp' }
--   exec (comp' e HALT) s
--   =     { c' = comp' e HALT in exec c' s = exec (comp' e HALT) s:
--           - define: comp e = comp' e HALT }

type Stack = [Maybe Int]

data Code = PUSH Int Code
          | THROW Code
          | ADD Code
          | CATCH Code
          | HALT deriving Show

comp :: Expr -> Code
comp e = comp' e HALT

comp' :: Expr -> Code -> Code
comp' (Val n) c = PUSH n c
comp' Throw c = THROW c
comp' (Add x y) c = comp' x (comp' y (ADD c))
comp' (Catch x h) c = comp' x (comp' h (CATCH c))

exec :: Code -> Stack -> Stack
exec (PUSH n c) s = exec c (Just n : s)
exec (THROW c) s = exec c (Nothing : s)
exec (ADD c) (my : mx : s) = exec c (sum : s)
                             where sum = do n <- mx
                                            m <- my
                                            return (n + m)
exec (CATCH c) (h : x : s) = exec c (caught : s)
                             where caught = case x of
                                              Just n -> Just n
                                              Nothing -> h
exec HALT s = s


-- For testing
expr :: Expr
expr = Add (Add (Catch (Add (Val 10) Throw) (Add (Val 20) (Val 15))) (Add (Val 11) (Val 51))) (Add (Val 1) (Add (Val 9) (Catch (Val 3) (Val 4))))

