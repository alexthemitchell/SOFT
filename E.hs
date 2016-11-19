{-# Language GADTs #-}
data Exp where
  --Types
  EInt  :: Int -> Exp
  EBool :: Bool -> Exp
  EChar :: Char -> Exp
  EStr  :: String -> Exp
  ELst  :: [Exp] -> Exp
  --Numeric Operations
  EAdd :: Exp -> Exp -> Exp  
  ESub :: Exp -> Exp -> Exp
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
  EAnd :: Exp -> Exp -> Exp 
  EOr  :: Exp -> Exp -> Exp
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
  EApp  :: Exp -> Exp -> Exp --Applies given function to expression  
  EClos :: Exp -> Exp --For parenthesis, brackets etc. 

value :: Exp -> Bool 
value (EInt _)    = True
value (EBool _)   = True
value (EChar _)   = True
value (EStr _)    = True
value (ELst _)    = True
value (EAdd _ _)  = False 
value (ESub _ _)  = False 
value (EMul _ _)  = False 
value (EDiv _ _)  = False
value (EMod _ _)  = False
value (EEql _ _)  = False 
value (ELtn _ _)  = False 
value (EGtn _ _)  = False
value (EGeq _ _)  = False
value (ELeq _ _)  = False 
value (EAnd _ _)  = False 
value (EOr  _ _)  = False
value (ENot _)    = False
value (EFst _)    = False 
value (ERst _)    = False 
value (EEmt _)    = False
value (ECons _ _) = False
value (ENil)      = False 
value (EVar _)    = True
value (EFunc _ _) = False 
value (EApp _ _)  = False
value (EClos _)   = True

step :: Exp -> Exp 
step (EInt  n) = EInt n
step (EBool b) = EBool b
step (EChar c) = EChar c
step (EStr  s) = EStr s
step (ELst  l) = ELst l
step (EAdd e1 e2) = (step e1) + (step e2)
step (ESub e1 e2) = (step e1) - (step e2) 
step (EMul e1 e2) = (step e1) * (step e2)
step (EDiv e1 e2) = (step e1) / (step e2) 
step (EMod e1 e2) = (step e1) `mod` (step e2)
step (EEql e1 e2) = EBool $ (step e1) == (step e2) 
step (EGtn e1 e2) = EBool $ (step e1) > (step e2)
step (ELtn e1 e2) = EBool $ (step e1) < (step e2) 
step (EGeq e1 e2) = EBool $ (step e1) >= (step e2) 
step (ELeq e1 e2) = EBool $ (step e1) <= (step e2)
step (EAnd b1 b2) = (EBool (step b1)) && (EBool (step b2)) --let and take a list?? 
step (EOr  b1 b2) = EBool $ (step b1) || (step b2) 
step (ENot e)     = not $ step e
step (EFst l)     = head $ step l

