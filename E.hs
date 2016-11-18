{-# Language GADTs #-}
data Exp where
  --Types
  EInt  :: Int -> Exp
  EBool :: Bool -> Exp
  EChar :: Char -> Exp
  EStr  :: String -> Exp
  ELst  :: [Exp] -> Exp
  --Numeric Operations
  ESub :: Exp -> Exp -> Exp  
  EAdd :: Exp -> Exp -> Exp
  EMul :: Exp -> Exp -> Exp 
  EDiv :: Exp -> Exp -> Exp 
  EMod :: Exp -> Exp -> Exp 
  --Comparative Operations
  EEql :: Exp -> Exp -> Exp 
  ELtn :: Exp -> Exp -> Exp 
  EGtn :: Exp -> Exp -> Exp 
  EGeq :: Exp -> Exp -> Exp 
  ELeq :: Exp -> Exp -> Exp
  --boolean operations
  EAnd :: Exp -> Exp 
  EOr  :: Exp -> Exp
  ENot :: Exp -> Exp
  --List Operations
  EFst  :: Exp -> Exp
  ERst  :: Exp -> Exp
  EEmt  :: Exp -> Exp
  ECons :: Exp -> Exp -> Exp --takes two ELsts
  ENil  :: Exp 
  -- Let Statements 
  EVar  :: String -> Exp --let  
  EFunc :: String -> Exp -> Exp --let for functions 
  --General Operations  
  EApp :: Exp -> Exp -> Exp --Applies given function to expression  
  EClosure :: Exp -> Exp --For parenthesis, brackets etc. 
{--
value :: Exp -> Bool :
  value (EInt _) = True
  value (EVar _) = True
  value(EBool _) = True
  value(EChar _) = True
  value(EString _) = True
  value(EApp _ _) = False
  value(EFunc _ _) = False


step :: Exp -> Exp 
step (EInt n) = EInt n
step (EBool b) = EBool b
step (EChar c) = EChar c
step (EString s) = EString s
step (EVar x)   = EVar x
step (EFunc x e) = EFunc x e
step (EApp e1 e2)
  | not $ value e1 = EApp (step e1) e2  --
  | not $ value e2 = EApp e1 (step e2)
  | otherwise =
          case e1 of
                | EAbs x e -> subst x e2 e
                |  _        -> error "Tried to apply non-function"



    --}


