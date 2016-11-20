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

instance Show Exp where
    show (EInt n)  = show n
    show (EBool b) = show b
    show (EChar c) = show c
    show (EStr s)  = show s
    show (EAdd e1 e2) = (show e1) ++ "+"   ++ (show e2)
    show (ESub e1 e2) = (show e1) ++ "-"   ++ (show e2)
    show (EMul e1 e2) = (show e1) ++ "*"   ++ (show e2)
    show (EDiv e1 e2) = (show e1) ++ "/"   ++ (show e2)
    show (EMod e1 e2) = (show e1) ++ "mod" ++ (show e2)
    show (EEql e1 e2) = (show e1) ++ "=="  ++ (show e2)
    show (ELtn e1 e2) = (show e1) ++ "<"   ++ (show e2)
    show (EGtn e1 e2) = (show e1) ++ ">"   ++ (show e2)
    show (ELeq e1 e2) = (show e1) ++ "<="  ++ (show e2)
    show (EGeq e1 e2) = (show e1) ++ ">="  ++ (show e2)
    show (EAnd e1 e2) = (show e1) ++ "and" ++ (show e2)
    show (EOr  e1 e2) = (show e1) ++ "or"  ++ (show e2)
    show (ENot e)     = "not"     ++          (show e)
    


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
step (EAdd (EInt e1) (EInt e2))   = step $ EInt $ e1 + e2
step (ESub (EInt e1) (EInt e2))   = step $ EInt $ e1 - e2
step (EMul (EInt e1) (EInt e2))   = step $ EInt $ e1 * e2
step (EDiv (EInt e1) (EInt e2))   = step $ EInt $ e1 `div` e2
step (EMod (EInt e1) (EInt e2))   = step $ EInt $ e1 `mod` e2
step (EEql (EInt e1) (EInt e2))   = step $ EBool $ e1 == e2 
step (EGtn (EInt e1) (EInt e2))   = step $ EBool $ e1 > e2
step (ELtn (EInt e1) (EInt e2))   = step $ EBool $ e1 < e2 
step (EGeq (EInt e1) (EInt e2))   = step $ EBool $ e1 >= e2 
step (ELeq (EInt e1) (EInt e2))   = step $ EBool $ e1 <= e2
step (EAnd (EBool b1) (EBool b2)) = step $ EBool $  b1 &&  b2 --let and take a list?? 
step (EOr  (EBool b1) (EBool b2)) = step $ EBool $ b1 || b2 
step (ENot (EBool b))             = step $ EBool $ not b
--step (EFst (ELst (ECons (EInt x) xs)))     = step $ EInt x
--step (EFst (ELst (EStr l)))     = step $ EInt $ head l
--step (EFst (ELst (EBool l)))     = step $ EInt $ head l
--step (ECons (EInt x) (ELst xs)) = step $ ELst $ (x:xs)  


evaluate :: Exp -> Exp
evaluate e 
  | not $ value e = evaluate (step e)
  | otherwise     =  e
