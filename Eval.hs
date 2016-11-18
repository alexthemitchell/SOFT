{-# LANGUAGE GADTs #-}

data Exp where
  EInt :: Int -> Exp              -- n
  EVar :: String -> Exp           -- x
  EApp :: Exp -> Exp -> Exp       -- e1 e2
  EAbs :: String -> Exp -> Exp    -- \x -> e
  EClosure :: Exp -> Exp          -- [[ e ]]

data EvalCtx where
  CHole :: EvalCtx                -- []
  EAppL :: Exp -> EvalCtx         -- E e
  EAppR :: Exp -> EvalCtx         -- v E

value :: Exp -> Bool
value (EInt _)   = True
value (EVar _)   = True
value (EApp _ _) = False
value (EAbs _ _) = True

step :: Exp -> Exp
step (EInt n)   = EInt n
step (EVar x)   = EVar x
step (EAbs x e) = EAbs x e
step (EApp e1 e2)
  | not $ value e1 = EApp (step e1) e2
  | not $ value e2 = EApp e1 (step e2)
  | otherwise =
    case e1 of
    | EAbs x e -> subst x e2 e
    | _        -> error "Tried to apply non-function"

-- [x/v] e -- "substitute v for x inside of e"
subst :: String -> Exp -> Exp -> Exp
subst x v (EInt n)     = EInt n
subst x v (EVar y)     = if x == y then v else EVar y
subst x v (EAbs y e)   = EAbs y (subst x v e)
subst x v (EApp e1 e2) = EApp (subst s v e1) (subst s v e2)

evaluate :: Exp -> Exp
evaluate e | not $ value e = evaluate (step e)
           | otherwise     = e

------------------------------------------------------------

type Env = [(String, Exp)]
type EnvStack = [Env]


-- g = \x -> x + 1

-- f (g 4)
-- f [[ v ]]
-- f v
