{-# Language GADTs #-}
module SOFTEval where

data Bop = 
  BAdd | BSub | BMul | BDiv | BMod | 
  BEql | BLtn | BGtn | BGeq | BLeq |
  BAnd | BOr 

data Exp where
  --Types
  EInt  :: Int -> Exp
  EFlt  :: Float -> Exp 
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

type Env = [(String, Exp)]
type EnvStack = [Env]

instance Show Exp where
    show (EInt n)  = show n
    show (EFlt f)  = show f
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
value (EFlt _)        = True
value (EBool _)       = True
value (EChar _)       = True
value (EStr _)        = True
value (ELst l)        = all value l --all :: (a -> Bool) -> [a] -> Bool
value (EErr _)        = True
value (EVar _)        = True
value (EClos _)       = True
value (EFunc _ _ _ _) = True
value (ELet _ _ _)    = True
value _           = False

step ::Env ->  Exp -> (Exp, Env)     
step e (EInt  n) = (EInt n, e)
step e (EFlt  f) = (EFlt f, e)
step e (EBool b) = (EBool b, e)
step e (EChar c) = (EChar c, e)
step e (EStr  s) = (EStr s, e)
step e (ELst  l) = (ELst l, e)
step v (EErr  e) = (EErr e, v)  
--step (EVar  s) = EVar s
step e (EBinop e1 op e2) 
  | not $ value e1 = (EBinop (step e e1) op e2, e) 
  | not $ value e2 = (EBinop e1 op (step e e2), e)
  | otherwise      =
     case (e1, op ,e2) of
       (EInt n1, BAdd, EInt n2) -> (EInt  $ n1 + n2, e) 
       (EFlt f1, BAdd, EFlt f2) -> (EFlt  $ f1 + f2, e)
       ( _     , BAdd, _      ) -> (EErr  $ "+ takes ints or floats", e)
       (EInt n1, BSub, EInt n2) -> (EInt  $ n1 - n2, e)     
       (EFlt f1, BSub, EFlt f2) -> (EFlt  $ f1 - f2, e)
       ( _     , BSub, _      ) -> (EErr  $ "- takes ints or floats", e)
       (EInt n1, BMul, EInt n2) -> (EInt  $ n1 * n2, e)
       (EFlt f1, BMul, EFlt f2) -> (EFlt  $ f1 * f2, e)
       ( _     , BMul, _      ) -> (EErr  $ "* takes ints or floats", e)
       (EInt n1, BDiv, EInt n2) -> (EInt  $ n1 `div` n2, e) 
       (EFlt f1, BDiv, EFlt f2) -> (EFlt  $ f1 / f2, e)
       ( _     , BDiv, _      ) -> (EErr  $ "/ takes ints or floats", e)
       (EInt n1, BMod, EInt n2) -> (EInt  $ n1 `mod` n2, e)      
       ( _     , BMod, _      ) -> (EErr  $ "mod takes int, int", e)
       (EInt n1, BEql, EInt n2) -> (EBool $ n1 == n2, e)
       (EFlt f1, BEql, EFlt f2) -> (EBool  $ f1 == f2, e)
       (EBool b1, BEql, EBool b2) -> (EBool $ b1 == b2, e)
       (EStr s1, BEql, EStr s2)   -> (EBool  $ s1 == s2, e)
       (EChar c1, BEql, EChar c2) -> (EBool $ c1 == c2, e)
       ( _     , BEql, _      ) -> (EErr  $ "== takes two of the same type", e) 
       (EInt n1, BLtn, EInt n2) -> (EBool $ n1 < n2, e) 
       ( _     , BLtn, _      ) -> (EErr  $ "< takes int, int", e)
       (EInt n1, BGtn, EInt n2) -> (EBool $ n1 > n2, e)      
       ( _     , BGtn, _      ) -> (EErr  $ "> takes int, int", e)
       (EInt n1, BLeq, EInt n2) -> (EBool $ n1 <= n2, e) 
       ( _     , BLeq, _      ) -> (EErr  $ "<= takes int, int", e)
       (EInt n1, BGeq, EInt n2) -> (EBool $ n1 >= n2, e) 
       ( _     , BGeq, _      ) -> (EErr  $ ">= takes int, int", e)
       (EBool b1, BAnd, EBool b2) -> (EBool $ b1 && b2, e)      
       ( _      , BAnd, _      ) -> (EErr  $ "and takes bool, bool", e)
       (EBool b1, BOr , EBool b2) -> (EBool $ b1 || b2, e) 
       ( _      , BOr , _      ) -> (EErr  $ "or takes bool, bool", e)
step e (ENot b)             
  |not $ value b = (ENot (step e b), e)
  |otherwise     = 
     case b of 
       (EBool b1) -> (EBool $ not b1, e)
       _          -> (EErr $ "not takes bool", e)
step e (EFst l)     
  | not $ value l = (EFst (step e l), e)
  | otherwise     =
     case l of 
      (ELst (x:_)) -> step e x
      _            -> (EErr $ "first takes a list", e)
step e (ERst l) 
  | not $ value l = (ELst (step e l), e)
  | otherwise     =
     case l of 
      (ELst (_:xs)) -> (ELst $ xs, e) 
      _             -> (EErr $ "rest takes a list", e)
step e (EEmt l) 
  |not $ value l = (EEmt (step e l), e)
  |otherwise     =
    case l of 
     ENil -> (EBool True, e)
     _    -> (EBool False, e)
step e (ECons v l)
  |not $ value v = (ECons (fst $ step e v) l, e)
  |not $ value l = (ECons v (fst $ step e l), e)
  |otherwise     =
    case l of 
      (ELst l) -> (ELst $ v:l, e)
      ENil     -> (ELst $ v: [], e)
      _        -> (EErr "cons takes a value and a list", e)
step e ENil = (ELst $ [], e)
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
  | not $ value e = evaluate $ fst (step [] e)
  | otherwise     =  e
