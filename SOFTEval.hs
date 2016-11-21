{-# Language GADTs #-}
module SOFTEval where

data Bop = 
  BAdd | BSub | BMul | BDiv | BMod | 
  BEql | BLtn | BGtn | BGeq | BLeq |
  BAnd | BOr 

data Exp where
  --Types
  EInt  :: Int -> Exp
  EBool :: Bool -> Exp
  EChar :: Char -> Exp
  EStr  :: String -> Exp
  ELst  :: [Exp] -> Exp
  EErr  :: String -> Exp
  --Numeric Operations
  EBinop :: Exp -> Bop -> Exp -> Exp
  ENot :: Exp -> Exp
  --List Operations
  EFst  :: Exp -> Exp
  ERst  :: Exp -> Exp
  EEmt  :: Exp -> Exp
  ECons :: Exp -> Exp -> Exp --takes two ELsts
  ENil  :: Exp 
  -- Let Statements 
{--
  let x = 5 in x + 1
  ELet "x" (EInt 5) (EAdd (EVar "x") (EInt 1))

  let f(x) = x+1 in f(0)
  EFunc f [x] (EAdd (EVar "x") (EInt 1)) (EApp f [0]))  
--}
  ELet  :: String -> Exp -> Exp -> Exp -- let x = e1 in e2
  EVar  :: String -> Exp --x 
  EFunc :: String -> [String] ->  Exp -> Exp -> Exp -- let f(x1, ..., xn) = e1 in e2
  --General Operations  
  EApp  :: String -> [Exp] -> Exp --Applies given function to expression  
  EClos :: Exp -> Exp --For parenthesis, brackets etc. 

instance Show Exp where
    show (EInt n)  = show n
    show (EBool b) = show b
    show (EChar c) = [c] 
    show (EStr s)  = s
    show (EErr e)  = "Error: " ++ e
    show (EBinop e1 op e2) = 
     case op of
      BAdd -> (show e1) ++ " + "   ++ (show e2)
      BSub -> (show e1) ++ " - "   ++ (show e2)
      BMul -> (show e1) ++ " * "   ++ (show e2)
      BDiv -> (show e1) ++ " / "   ++ (show e2)
      BMod -> (show e1) ++ " mod " ++ (show e2)
      BEql -> (show e1) ++ " == "  ++ (show e2)
      BLtn -> (show e1) ++ " < "   ++ (show e2)
      BGtn -> (show e1) ++ " > "   ++ (show e2)
      BLeq -> (show e1) ++ " <= "  ++ (show e2)
      BGeq -> (show e1) ++ " >= "  ++ (show e2)
      BAnd -> (show e1) ++ " and " ++ (show e2)
      BOr  -> (show e1) ++ " or "  ++ (show e2)
    show (ENot e)     = "not " ++ (show e)
    show (EFst l)     = "first " ++ (show l)
    show (ERst l)     = "[" ++ (show l) ++ "]"
    show ENil         = "[]"
    show (ECons v l) = (show v) ++ ":" ++ (show l)
    show (EEmt l)     = "empty " ++ (show l)         


value :: Exp -> Bool 
value (EInt _)        = True
value (EBool _)       = True
value (EChar _)       = True
value (EStr _)        = True
value (ELst l)        = all value l --all :: (a -> Bool) -> [a] -> Bool
value (EErr _)        = True
value (EVar _)        = True
value (EClos _)       = True
value (EFunc _ _ _ _) = True
value (ELet _ _ _)    = True
value (EVar _)        = True
value _           = False
--Env ->  Exp -> (Exp, Env)
     
step ELet s v e1
step :: Exp -> Exp 
step (EInt  n) = EInt n
step (EBool b) = EBool b
step (EChar c) = EChar c
step (EStr  s) = EStr s
step (ELst  l) = ELst l
step (EErr  e) = EErr e  
step (EVar  s) = EVar s
step (EBinop e1 op e2) 
  | not $ value e1 = EBinop (step e1) op e2 
  | not $ value e2 = EBinop e1 op (step e2)
  | otherwise      =
     case (e1, op ,e2) of
       (EInt n1, BAdd, EInt n2) -> EInt  $ n1 + n2 
       ( _     , BAdd, _      ) -> EErr  $ "+ takes int, int"
       (EInt n1, BSub, EInt n2) -> EInt  $ n1 - n2      
       ( _     , BSub, _      ) -> EErr  $ "- takes int, int"
       (EInt n1, BMul, EInt n2) -> EInt  $ n1 * n2 
       ( _     , BMul, _      ) -> EErr  $ "* takes int, int"
       (EInt n1, BDiv, EInt n2) -> EInt  $ n1 `div` n2 
       ( _     , BDiv, _      ) -> EErr  $ "/ takes int, int"
       (EInt n1, BMod, EInt n2) -> EInt  $ n1 `mod` n2      
       ( _     , BMod, _      ) -> EErr  $ "mod takes int, int"
       (EInt n1, BEql, EInt n2) -> EBool $ n1 == n2
       ( _     , BEql, _      ) -> EErr  $ "== takes int, int"
       (EInt n1, BLtn, EInt n2) -> EBool $ n1 < n2 
       ( _     , BLtn, _      ) -> EErr  $ "< takes int, int"
       (EInt n1, BGtn, EInt n2) -> EBool $ n1 > n2      
       ( _     , BGtn, _      ) -> EErr  $ "> takes int, int"
       (EInt n1, BLeq, EInt n2) -> EBool $ n1 <= n2 
       ( _     , BLeq, _      ) -> EErr  $ "<= takes int, int"
       (EInt n1, BGeq, EInt n2) -> EBool $ n1 >= n2 
       ( _     , BGeq, _      ) -> EErr  $ ">= takes int, int"
       (EBool b1, BAnd, EBool b2) -> EBool $ b1 && b2      
       ( _      , BAnd, _      ) -> EErr  $ "and takes bool, bool"
       (EBool b1, BOr , EBool b2) -> EBool $ b1 || b2 
       ( _      , BOr , _      ) -> EErr  $ "or takes bool, bool"
step (ENot b)             
  |not $ value b = ENot (step b)
  |otherwise     = 
     case b of 
       (EBool b1) -> EBool $ not b1
       _          -> EErr $ "not takes bool"
step (EFst l)     
  | not $ value l = EFst (step l)
  | otherwise     =
     case l of 
      (ELst (x:_)) -> step x
      _            -> EErr $ "first takes a list"
step (ERst l) 
  | not $ value l = EFst (step l)
  | otherwise     =
     case l of 
      (ELst (_:xs)) -> ELst $ xs 
      _             -> EErr $ "rest takes a list"
step (EEmt l) 
  |not $ value l = EEmt (step l)
  |otherwise     =
    case l of 
     ENil -> EBool True
     _    -> EBool False
step (ECons v l)
  |not $ value v = ECons (step v) l
  |not $ value l = ECons v (step l)
  |otherwise     =
    case l of 
      (ELst l) -> ELst $ v:l
      ENil     -> ELst $ v: []
      _        -> EErr "cons takes a value and a list"
step ENil = ELst $ []
{--
step (EApp e1 e2)
  | not $ value e1 = EApp (step e1) e2
  | not $ value e2 = EApp e1 (step e2)
  | otherwise      =
    case e1 of
     (ELet s v e1)     -> subst s v e1 
     (EFunc s l s1 s2) -> subst s v s2
     
step (ELet s v e1)
  | not $ value v  = ELet (step v) e1
  | not $ value e1 = ELet v (step e1)
  | otherwise      =
    case 
step EFunc s (x:xs) e1 e2
  
subst :: String -> Exp -> Exp -> Exp
subst x v (EAdd ) = 
--}
{--
  let x = 5 in x + 1
  ELet "x" (EInt 5) (EAdd (EVar "x") (EInt 1))

  let f(x) = x+1 in f(0)
  EFunc f [x] (EAdd (EVar "x") (EInt 1)) (EApp f [0]))  

  ELet  :: String -> Exp -> Exp -> Exp -- let x = e1 in e2
  EVar  :: String -> Exp --x 
  EFunc :: String -> [String] ->  Exp -> Exp -> Exp -- let f(x1, ..., xn) = e1 in e2
--}
evaluate :: Exp -> Exp
evaluate e 
  | not $ value e = evaluate (step e)
  | otherwise     =  e
