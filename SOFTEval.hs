{-# Language GADTs #-}
module SOFTEval where
import SOFTLexer
import Prelude hiding (fst, snd)
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
  ELet  :: String -> Exp -> Exp -- let x = e1
  EVar  :: String -> Exp --x
  EPar  :: String -> Exp
  EFunc :: String -> [String] ->  Exp -> Exp -- let f(x1, ..., xn) = e1
  --General Operations
  EApp  :: String -> [Exp] -> Exp --Applies given function to expressio
  EExpl :: Exp -> Exp
  EIf   :: Exp -> Exp -> Exp -> Exp
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
    show (ERst l)     = show l 
    show (ECons v l) = (show v) ++ ":" ++ (show l)
    show (EEmt l)     = "empty " ++ (show l)
    show (EIf b e1 e2) = "if " ++ (show b) ++ " then " ++ (show e1) ++ " else " ++ (show e2)
    show (EFunc s p e)    = "Function " ++ show s ++ show p ++" = "++ show e
    show (EApp x e) = x ++ "( " ++ (show e) ++ ")"
    show (EExpl e)  = (show e)

value :: Exp -> Bool
value (EInt _)        = True
value (EFlt _)        = True
value (EBool _)       = True
value (EChar _)       = True
value (EStr _)        = True
value (ELst l)        = True--all value l --all :: (a -> Bool) -> [a] -> Bool
value (EErr _)        = True
value (EClos _)       = True
value _           = False

type Buffer = [String]

step :: Bool-> Buffer -> Env ->  Exp -> (Exp, Env, Buffer)
step d pb e (EInt  n) = (EInt n, e, if d then (show n):pb else pb)
step d pb e (EFlt  f) = (EFlt f, e, if d then (show f):pb else pb)
step d pb e (EBool b) = (EBool b, e, if d then (show b):pb else pb)
step d pb e (EChar c) = (EChar c, e, if d then (show c):pb else pb)
step d pb e (EStr  s) = (EStr s, e, if d then (show s):pb else pb)
step d pb e (ELst  l) = (ELst l, e, if d then (show l):pb else pb)
step d pb v (EErr  e) = (EErr e, v, if d then (show e):pb else pb)
step d pb e (EVar  s) = (find s e, e, if d then s:(show(find s e)):pb else pb) --returns value associated with variable
step d pb e (EBinop e1 op e2)
  | not $ value e1 = (EBinop (fst $ step d pb e e1) op e2, e, if d then (show (EBinop e1 op e2)):pb else pb )
  | not $ value e2 = (EBinop e1 op (fst $ step d pb e e2), e, if d then (show (EBinop e1 op e2)):pb else pb )
  | otherwise      =
     case (e1, op ,e2) of
       (EInt n1, BAdd, EInt n2) -> (EInt $ n1 + n2, e, (if d then ((show n1) ++ "+" ++ (show n2)):pb else pb))
       (EInt n1, BAdd, EFlt f2) -> (EFlt  $ (fromIntegral n1) + f2, e, (if d then ((show n1) ++ "+" ++ (show f2)):pb else pb))
       (EFlt f1, BAdd, EInt n2) -> (EFlt  $ f1 + (fromIntegral n2),e, (if d then ((show f1) ++ "+" ++ (show n2)):pb else pb))
       (EFlt f1, BAdd, EFlt f2) -> (EFlt  $ f1 + f2,e, (if d then ((show f1) ++ "+" ++ (show f2)):pb else pb))
       ( _     , BAdd, _      ) -> (EErr  $ "+ takes ints or floats",e,pb)
       (EInt n1, BSub, EInt n2) -> (EInt  $ n1 - n2, e, (if d then ((show n1) ++ "-" ++ (show n2)):pb else pb))
       (EFlt f1, BSub, EInt n2) -> (EFlt  $ f1 - (fromIntegral n2),e, (if d then ((show f1) ++ "-" ++ (show n2)):pb else pb))
       (EInt n1, BSub, EFlt f2) -> (EFlt  $ (fromIntegral n1) - f2,e, (if d then ((show n1) ++ "-" ++ (show f2)):pb else pb))
       (EFlt f1, BSub, EFlt f2) -> (EFlt  $ f1 - f2,e, (if d then ((show f1) ++ "-" ++ (show f2)):pb else pb))
       ( _     , BSub, _      ) -> (EErr  $ "- takes ints or floats",e, pb)
       (EInt n1, BMul, EInt n2) -> (EInt  $ n1 * n2,e, (if d then ((show n1) ++ "*" ++ (show n2)):pb else pb))
       (EFlt f1, BMul, EInt n2) -> (EFlt $ f1 * (fromIntegral n2),e, (if d then ((show f1) ++ "*" ++ (show n2)):pb else pb))
       (EInt n1, BMul, EFlt f2) -> (EFlt  $ (fromIntegral n1) * f2,e, (if d then ((show n1) ++ "*" ++ (show f2)):pb else pb))
       (EFlt f1, BMul, EFlt f2) -> (EFlt  $ f1 * f2,e, (if d then ((show f1) ++ "*" ++ (show f2)):pb else pb))
       ( _     , BMul, _      ) -> (EErr  $ "* takes ints or floats",e, pb)
       (EInt n1, BDiv, EInt n2) -> (EInt  $ n1 `div` n2,e, (if d then ((show n1) ++ "/" ++ (show n2)):pb else pb))
       (EFlt f1, BDiv, EFlt f2) -> (EFlt  $ f1 / f2,e, (if d then ((show f1) ++ "/" ++ (show f2)):pb else pb))
       (EInt n1, BDiv, EFlt f2) -> (EFlt  $ (fromIntegral n1) / f2,e, (if d then ((show n1) ++ "/" ++ (show f2)):pb else pb))
       (EFlt f1, BDiv, EInt n2) -> (EFlt  $ f1 / (fromIntegral n2),e, (if d then ((show f1) ++ "/" ++ (show n2)):pb else pb))
       ( _     , BDiv, _      ) -> (EErr  $ "/ takes ints or floats",e,pb)
       (EInt n1, BMod, EInt n2) -> (EInt  $ n1 `mod` n2,e, (if d then ((show n1) ++ "mod" ++ (show n2)):pb else pb))
       ( _     , BMod, _      ) -> (EErr  $ "mod takes int, int",e,pb)
       (EInt n1, BEql, EInt n2) -> (EBool $ n1 == n2,e, (if d then ((show n1) ++ "==" ++ (show n2)):pb else pb))
       (EFlt f1, BEql, EFlt f2) -> (EBool  $ f1 == f2,e, (if d then ((show f1) ++ "==" ++ (show f2)):pb else pb))
       (EBool b1, BEql, EBool b2) -> (EBool $ b1 == b2,e, (if d then ((show b1) ++ "==" ++ (show b2)):pb else pb))
       (EStr s1, BEql, EStr s2)   -> (EBool  $ s1 == s2,e, (if d then ((show s1) ++ "==" ++ (show s2)):pb else pb))
       (EChar c1, BEql, EChar c2) -> (EBool $ c1 == c2,e, (if d then ((show c1) ++ "==" ++ (show c2)):pb else pb))
       ( _     , BEql, _      ) -> (EErr  $ "== takes two of the same type",e,pb)
       (EInt n1, BLtn, EInt n2) -> (EBool $ n1 < n2,e, (if d then ((show n1) ++ "<" ++ (show n2)):pb else pb))
       ( _     , BLtn, _      ) -> (EErr  $ "< takes int, int",e,pb)
       (EInt n1, BGtn, EInt n2) -> (EBool $ n1 > n2,e, (if d then ((show n1) ++ ">" ++ (show n2)):pb else pb))
       ( _     , BGtn, _      ) -> (EErr  $ "> takes int, int",e,pb)
       (EInt n1, BLeq, EInt n2) -> (EBool $ n1 <= n2,e, (if d then ((show n1) ++ "≤" ++ (show n2)):pb else pb))
       ( _     , BLeq, _      ) -> (EErr  $ "<= takes int, int",e,pb)
       (EInt n1, BGeq, EInt n2) -> (EBool $ n1 >= n2,e, (if d then ((show n1) ++ "≥" ++ (show n2)):pb else pb))
       ( _     , BGeq, _      ) -> (EErr  $ ">= takes int, int",e,pb)
       (EBool b1, BAnd, EBool b2) -> (EBool $ b1 && b2,e, (if d then ((show b1) ++ "and" ++ (show b2)):pb else pb))
       ( _      , BAnd, _      ) -> (EErr  $ "and takes bool, bool",e,pb)
       (EBool b1, BOr , EBool b2) -> (EBool $ b1 || b2,e, (if d then ((show b1) ++ "or" ++ (show b2)):pb else pb))
       ( _      , BOr , _      ) -> (EErr  $ "or takes bool, bool",e,pb)

step d pb e (ENot b)
  |not $ value b = (ENot (fst $ step d pb e b),e, if d then (show (ENot b):pb) else pb)
  |otherwise     =
     case b of
       (EBool b1) -> (EBool $ not b1,e, if d then (show (ENot b)):pb else pb)
       _          -> (EErr $ "not takes bool", e, pb)
step d pb e (EFst l)
  | not $ value l = (EFst (fst $ step d pb e l), e, if d then (show l):pb else pb)
  | otherwise     =
     case l of
      (ELst (x:_)) -> step d pb e x
      ENil         -> (ENil, e, if d then (show ENil):pb else pb)
      _            -> (EErr $ "first takes a list", e, pb)
step d pb e (ERst l)
  | not $ value l = (ERst (fst $ step d pb e l), e, if d then (show l):pb else pb)
  | otherwise     =
     case l of
      (ELst (_:xs)) -> (ELst $ xs, e, if d then (show xs):pb else pb)
      ENil          -> (ENil, e, if d then (show ENil):pb else pb)
      _             -> (EErr $ "rest takes a list", e, pb)
step d pb e (EEmt l)
  |not $ value l = (EEmt (fst $ step d pb e l), e, if d then (show l):pb else pb)
  |otherwise     =
    case l of
     ELst [] -> (EBool True, e, if d then (show l):pb else pb)
     ENil -> (EBool True, e, if d then (show ENil):pb else pb)
     _    -> (EBool False, e, pb)
step d pb e (ECons v l)
  |not $ value v = (ECons (fst $ step d pb e v) l, e, if d then (show (ECons v l)):pb else pb)
  |not $ value l = (ECons v (fst $ step d pb e l), e, if d then (show (ECons v l)):pb else pb)
  |otherwise     =
    case l of
      (ELst l) -> (ELst $ v:l, e, if d then (show l):pb else pb)
      ENil     -> (ELst $ v: [], e, if d then (show ENil):pb else pb)
      _        -> (EErr "cons takes a value and a list", e, pb)
step d pb e ENil = (ELst $ [],e, if d then (show ENil):pb else pb )
step d pb e (EIf b e1 e2)
  | not $ value b = step d pb e (EIf (fst $ step d pb e b) e1 e2)
  | otherwise     = 
     case b of 
      EBool b1 -> if b1 then step d pb e e1 else step d pb e e2
      _        -> (EErr "If not given a boolean value", e, pb)
--Applies defined function
step d pb e (EApp s lv) =
  case find s e of 
   (EFunc f lp e1) -> (eApply d pb lp lv e1 e, e, if d then (show (EApp s lv)):pb else pb)
   _               -> (EErr $  "function" ++ s  ++ "is not declared", e, pb)
--call for variable declaration
step d pb e (ELet s v)
  | existsIn s e   = (ENil, findAndReplace s v e, if d then (s ++ "declared as" ++ (show v)):pb else pb)
  | value v        = (ENil, (s,v):e, if d then (s ++ "declared as" ++ (show v)):pb else pb)
  | otherwise      =
     case v of
      (ELet _ _)    -> (EErr "cannot assign variable to another variable declaration", e, pb)
      (EFunc _ _ _) -> (EErr "cannot assign variable to a function declaration", e, pb)
      _               -> (ELet s (fst $ step d pb e v) , e, pb)
--call for function declaration
step d pb e (EFunc s l e1)
  | value e1  = (EErr $ "cannot assign function to value", e, pb)
  | existsIn s e = (ENil, findAndReplace s (EFunc s l e1) e, if d then (s ++ "declared as " ++ (show e1)):pb else pb) 
  | otherwise = (ENil, (s, (EFunc s l e1)):e, pb)

eApply :: Bool -> Buffer -> [String] -> [Exp] -> Exp -> Env -> Exp
eApply d pb s v exp env 
  | not $ all value v = eApply d pb s (map (\x -> if not $ value x then fst $ step d pb env x else x) v) exp env
  | value exp         = exp
  | otherwise         = eApply d pb s v (fst $ step d pb ((zip s v)++env) exp) env

existsIn :: String -> Env -> Bool
existsIn _ []   = False
existsIn s ((s1, _):xs)
  | s==s1     = True
  | otherwise = existsIn s xs

find :: String -> Env -> Exp
find _ []       = EErr "Variable has not been declared"
find s ((s1, v1):xs)
  | s == s1   = v1
  | otherwise = find s xs

findAndReplace :: String -> Exp -> Env -> Env  
findAndReplace s v ((s1, v1):xs)
  | s == s1   = (s1, v):xs
  | otherwise = findAndReplace s v xs

fst :: (a, b, c) -> a
fst (x, y, z) = x

snd :: (a, b, c) -> b
snd (x, y, z) = y

thd :: (a, b, c) -> c
thd (x, y, z) = z

  
evaluate :: Bool -> Env -> Exp -> Buffer  -> (Exp, Env, Buffer)
evaluate d env exp pb
  | not $ value exp = (\(ex, en, pb) -> evaluate d en ex pb) (step d pb env exp)
  | otherwise     =  (exp, env, pb)
