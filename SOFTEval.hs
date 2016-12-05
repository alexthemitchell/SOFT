{-# Language GADTs #-}
module SOFTEval where
import SOFTLexer
--import Data.Global
import Data.IORef

-- Error handling --
-- The exception Monad below is taken from:
-- https://www.haskell.org/happy/doc/html/sec-monads.html#sec-exception

parseError :: [Token] -> E a
parseError tokens = failE "Parse error"

data E a = Ok a | Failed String

instance (Show a) => Show (E a) where
    show (Ok a)  = show a
    show (Failed s) = s

thenE :: E a -> (a -> E b) -> E b
m `thenE` k =
  case m of Ok a -> k a
            Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
catchE m k =
  case m of Ok a -> Ok a
            Failed e -> k e

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
  ELet  :: String -> Exp -> Exp -- let x = e1
  EVar  :: String -> Exp --x
  EPar  :: String -> Exp
  EFunc :: String -> [String] ->  Exp -> Exp -- let f(x1, ..., xn) = e1
  --General Operations
  EApp  :: String -> [Exp] -> Exp --Applies given function to expressio
  EIf   :: Exp -> Exp -> Exp -> Exp
 -- EApp  :: Exp -> [Exp] -> Exp
  EClos :: Exp -> Exp --For parenthesis, brackets etc.

type Env = [(String, Exp)]
type EnvStack = [Env]

instance Show Exp where
    show (EInt n)  = show n
    show (EFlt f)  = show f
    show (EBool b)
      | b == True = "true"
      | b == False = "false"
    show (ELet n v)= show n ++ " is " ++ show v
    show (EChar c) = [c]
    show (EStr s)  = s
    show (ELst []) ="[]"
    show (ELst l) = show l
    show (EVar v) = show v 
    show (EPar p) = show p
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
    show ENil         = ""
    show (ERst l)     = "[" ++ (show l) ++ "]"
    show (ECons v l) = (show v) ++ ":" ++ (show l)
    show (EEmt l)     = "empty " ++ (show l)
    show (EIf b e1 e2) = "if" ++ (show b) ++ "then" ++ (show e1) ++ "else" ++ (show e2)
    show (EFunc s p e)    = "Function " ++ show s ++ show p ++" = "++ show e


value :: Exp -> Bool
value (EInt _)        = True
value (EFlt _)        = True
value (EBool _)       = True
value (EChar _)       = True
value (EStr _)        = True
value (ELst l)        = all value l --all :: (a -> Bool) -> [a] -> Bool
value (EErr _)        = True
value (EVar _)        = False 
value (EClos _)       = True
value (EFunc _ _ _) = False
value (ELet _ _)    = False
value _           = False

find :: String -> Env -> Exp
find _ []       = EErr "Variable has not been declared"
--find "" []       = EErr "Function has not been declared"
find s ((s1, v1):xs)
  | s == s1   = v1
  | otherwise = find s xs

step :: Env ->  Exp -> (Exp, Env)
step e (EInt  n) = (EInt n, e)
step e (EFlt  f) = (EFlt f, e)
step e (EBool b) = (EBool b, e)
step e (EChar c) = (EChar c, e)
step e (EStr  s) = (EStr s, e)
step e (ELst  l) = (ELst l, e)
step v (EErr  e) = (EErr e, v)
step e (EVar  s) = step e (find s e)
step e (EPar  s) = step e (find s e)
--returns value associated with variable
step e (EBinop e1 op e2)
  | not $ value e1 = (EBinop (fst $ step e e1) op e2, e)
  | not $ value e2 = (EBinop e1 op (fst $ step e e2), e)
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
  |not $ value b = (ENot (fst $ step e b), e)
  |otherwise     =
     case b of
       (EBool b1) -> (EBool $ not b1, e)
       _          -> (EErr $ "not takes bool", e)
step e (EFst l)
  | not $ value l = (EFst (fst $ step e l), e)
  | otherwise     =
     case l of
      (ELst (x:_)) -> step e x
      _            -> (EErr $ "first takes a list", e)
step e (ERst l)
  | not $ value l = (ELst [(fst $ step e l)], e)
  | otherwise     =
     case l of
      (ELst (_:xs)) -> (ELst $ xs, e)
      _             -> (EErr $ "rest takes a list", e)
step e (EEmt l)
  |not $ value l = (EEmt (fst $ step e l), e)
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
step e (EApp s lv) =
  case find s e of 
   (EFunc f lp e1) -> (eApply lp lv e1 e, e)
   _               -> (EErr $ "function" ++ s  ++ "is not declared", e)
--call for variable declaration
step e (ELet s v)
  | value v        = (v, (s,v):e)
  | otherwise      =
     case v of
      (ELet _ _)    -> (EErr "cannot assign variable to another variable declaration", e)
      (EFunc _ _ _) -> (EErr "cannot assign variable to a function declaration", e)
      _               -> (ELet s (fst $ step e v) , e)
--call for function declaration
step e (EFunc s l e1)
  | value e1  = (EErr $ "cannot assign function to value", e)
  | otherwise = (ENil, (s, (EFunc s l e1)):e)
step e (EIf b e1 e2)
  | not $ value b = step e (EIf (fst $ step e b) e1 e2)
  | otherwise     = 
     case b of 
      EBool b1 -> if b1 then step e e1 else step e e2
      _        -> (EErr "If not given a boolean value", e)
eApply :: [String] -> [Exp] -> Exp -> Env -> Exp
eApply s v exp env 
  | not $ all value v = eApply s (map (\x -> if not $ value x then fst $ step env x else x) v) exp env
  | value exp         = exp
  | otherwise         = eApply s v (fst $ step ((zip s v)++env) exp) env
{--
  let x = 5 in x + 1
  ELet "x" (EInt 5) (EAdd (EVar "x") (EInt 1))

  let f(x) = x+1 in f(0)
  EFunc f [x] (EAdd (EVar "x") (EInt 1)) (EApp f [0]))
  EApp  :: String -> [Exp] -> Exp
  ELet  :: String -> Exp -> Exp -> Exp -- let x = e1 in e2
  EVar  :: String -> Exp --x
  EFunc :: String -> [String] ->  Exp -> Exp -> Exp -- let f(x1, ..., xn) = e1 in e2
	--}
  
evaluate :: Env -> Exp -> (Exp, Env)
evaluate env exp
  | not $ value exp = (\(ex, en) -> evaluate en ex) (step env exp)
  | otherwise     =  (exp, env)
