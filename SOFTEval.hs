{-# Language GADTs #-}
module SOFTEval where
import SOFTLexer
import Prelude
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
  EApp  :: String -> [Exp] -> Exp --Applies given function to expression
  EPrint:: Exp -> Exp
  EIf   :: Exp -> Exp -> Exp -> Exp
  EClos :: Exp -> Exp --For parenthesis, brackets etc.

type Env = [(String, Exp)]
type EnvStack = [Env]

-- Debug to print buffer, depth
type DebugEnv = (Bool, Int)

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
    show (EVar v) = v
    show (EPar p) = show p
    show (EErr e)  = "Error: " ++ e
    show (EBinop e1 op e2) =
     case op of
      BAdd -> "(" ++ (show e1) ++ " + "   ++ (show e2) ++ ")"
      BSub -> "(" ++ (show e1) ++ " - "   ++ (show e2) ++ ")"
      BMul -> "(" ++ (show e1) ++ " * "   ++ (show e2) ++ ")"
      BDiv -> "(" ++ (show e1) ++ " / "   ++ (show e2) ++ ")"
      BMod -> "(" ++ (show e1) ++ " mod " ++ (show e2) ++ ")"
      BEql -> "(" ++ (show e1) ++ " == "  ++ (show e2) ++ ")"
      BLtn -> "(" ++ (show e1) ++ " < "   ++ (show e2) ++ ")"
      BGtn -> "(" ++ (show e1) ++ " > "   ++ (show e2) ++ ")"
      BLeq -> "(" ++ (show e1) ++ " <= "  ++ (show e2) ++ ")"
      BGeq -> "(" ++ (show e1) ++ " >= "  ++ (show e2) ++ ")"
      BAnd -> "(" ++ (show e1) ++ " and " ++ (show e2) ++ ")"
      BOr  -> "(" ++ (show e1) ++ " or "  ++ (show e2) ++ ")"
    show (ENot e)     = "not(" ++ (show e) ++ ")"
    show (EFst l)     = "first(" ++ (show l) ++ ")"
    show ENil         = ""
    show (ERst l)     = "rest(" ++ show l ++ ")"
    show (ECons v l) = (show v) ++ ":" ++ (show l)
    show (EEmt l)     = "empty(" ++ (show l) ++ ")"
    show (EIf b e1 e2) = "if " ++ (show b) ++ " then " ++ (show e1) ++ " else " ++ (show e2)
    show (EFunc s p e)    = "Function " ++ show s ++ show p ++" = "++ show e
    show (EApp x e) = x ++ "("++ (show e) ++ ")"

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

step :: DebugEnv -> Buffer -> Env ->  Exp -> (Exp, Env, Buffer)
step (db,_) pb e (EInt  n) = (EInt n, e, if db then (show n):pb else pb)
step (db,_) pb e (EFlt  f) = (EFlt f, e, if db then (show f):pb else pb)
step (db,_) pb e (EBool b) = (EBool b, e, if db then (show b):pb else pb)
step (db,_) pb e (EChar c) = (EChar c, e, if db then (show c):pb else pb)
step (db,_) pb e (EStr  s) = (EStr s, e, if db then (show s):pb else pb)
step (db,_) pb e (ELst  l) = (ELst l, e, if db then (show l):pb else pb)
step (db,_) pb v (EErr  e) = (EErr e, v, if db then (show e):pb else pb)
step (db,_) pb e (EVar  s) = (find s e, e, if db then s:pb else pb) --returns value associated with variable
step (db,dp) pb e (EBinop e1 op e2)
  | not $ value e1 = let (ex,en,bf) = evaluate (db,dp+1) e e1 [] in 
    (EBinop ex op e2, e, if db then ((show (EBinop e1 op e2)):bf)++pb else pb )
  | not $ value e2 = let (ex,en,bf) = evaluate (db,dp+1) e e2 [] in
    (EBinop e1 op ex, e, if db then ((show (EBinop e1 op e2)):bf)++pb else pb )
  | otherwise      =
     case (e1, op ,e2) of
       (EInt n1, BAdd, EInt n2) -> (EInt  $ n1 + n2, e, (if db then ((show n1) ++ "+" ++ (show n2)):pb else pb))
       (EInt n1, BAdd, EFlt f2) -> (EFlt  $ (fromIntegral n1) + f2, e, (if db then ((show n1) ++ "+" ++ (show f2)):pb else pb))
       (EFlt f1, BAdd, EInt n2) -> (EFlt  $ f1 + (fromIntegral n2),e, (if db then ((show f1) ++ "+" ++ (show n2)):pb else pb))
       (EFlt f1, BAdd, EFlt f2) -> (EFlt  $ f1 + f2,e, (if db then ((show f1) ++ "+" ++ (show f2)):pb else pb))
       (ELst l1, BAdd, ELst l2) -> (ELst  $ l1 ++ l2,e, (if db then ((show l1) ++ "+" ++ (show l2)):pb else pb))
       ( _     , BAdd, _      ) -> (EErr  $ "+ takes ints or floats",e,pb)
       (EInt n1, BSub, EInt n2) -> (EInt  $ n1 - n2, e, (if db then ((show n1) ++ "-" ++ (show n2)):pb else pb))
       (EFlt f1, BSub, EInt n2) -> (EFlt  $ f1 - (fromIntegral n2),e, (if db then ((show f1) ++ "-" ++ (show n2)):pb else pb))
       (EInt n1, BSub, EFlt f2) -> (EFlt  $ (fromIntegral n1) - f2,e, (if db then ((show n1) ++ "-" ++ (show f2)):pb else pb))
       (EFlt f1, BSub, EFlt f2) -> (EFlt  $ f1 - f2,e, (if db then ((show f1) ++ "-" ++ (show f2)):pb else pb))
       ( _     , BSub, _      ) -> (EErr  $ "- takes ints or floats",e, pb)
       (EInt n1, BMul, EInt n2) -> (EInt  $ n1 * n2,e, (if db then ((show n1) ++ "*" ++ (show n2)):pb else pb))
       (EFlt f1, BMul, EInt n2) -> (EFlt  $ f1 * (fromIntegral n2),e, (if db then ((show f1) ++ "*" ++ (show n2)):pb else pb))
       (EInt n1, BMul, EFlt f2) -> (EFlt  $ (fromIntegral n1) * f2,e, (if db then ((show n1) ++ "*" ++ (show f2)):pb else pb))
       (EFlt f1, BMul, EFlt f2) -> (EFlt  $ f1 * f2,e, (if db then ((show f1) ++ "*" ++ (show f2)):pb else pb))
       ( _     , BMul, _      ) -> (EErr  $ "* takes ints or floats",e, pb)
       (EInt n1, BDiv, EInt n2) -> (EInt  $ n1 `div` n2,e, (if db then ((show n1) ++ "/" ++ (show n2)):pb else pb))
       (EFlt f1, BDiv, EFlt f2) -> (EFlt  $ f1 / f2,e, (if db then ((show f1) ++ "/" ++ (show f2)):pb else pb))
       (EInt n1, BDiv, EFlt f2) -> (EFlt  $ (fromIntegral n1) / f2,e, (if db then ((show n1) ++ "/" ++ (show f2)):pb else pb))
       (EFlt f1, BDiv, EInt n2) -> (EFlt  $ f1 / (fromIntegral n2),e, (if db then ((show f1) ++ "/" ++ (show n2)):pb else pb))
       ( _     , BDiv, _      ) -> (EErr  $ "/ takes ints or floats",e,pb)
       (EInt n1, BMod, EInt n2) -> (EInt  $ n1 `mod` n2,e, (if db then ((show n1) ++ "mod" ++ (show n2)):pb else pb))
       ( _     , BMod, _      ) -> (EErr  $ "mod takes int, int",e,pb)
       (EInt n1, BEql, EInt n2) -> (EBool $ n1 == n2,e, (if db then ((show n1) ++ "==" ++ (show n2)):pb else pb))
       (EFlt f1, BEql, EFlt f2) -> (EBool  $ f1 == f2,e, (if db then ((show f1) ++ "==" ++ (show f2)):pb else pb))
       (EBool b1, BEql, EBool b2) -> (EBool $ b1 == b2,e, (if db then ((show b1) ++ "==" ++ (show b2)):pb else pb))
       (EStr s1, BEql, EStr s2)   -> (EBool  $ s1 == s2,e, (if db then ((show s1) ++ "==" ++ (show s2)):pb else pb))
       (EChar c1, BEql, EChar c2) -> (EBool $ c1 == c2,e, (if db then ((show c1) ++ "==" ++ (show c2)):pb else pb))
       ( _     , BEql, _      ) -> (EErr  $ "== takes two of the same type",e,pb)
       (EInt n1, BLtn, EInt n2) -> (EBool $ n1 < n2,e, (if db then ((show n1) ++ "<" ++ (show n2)):pb else pb))
       ( _     , BLtn, _      ) -> (EErr  $ "< takes int, int",e,pb)
       (EInt n1, BGtn, EInt n2) -> (EBool $ n1 > n2,e, (if db then ((show n1) ++ ">" ++ (show n2)):pb else pb))
       ( _     , BGtn, _      ) -> (EErr  $ "> takes int, int",e,pb)
       (EInt n1, BLeq, EInt n2) -> (EBool $ n1 <= n2,e, (if db then ((show n1) ++ "≤" ++ (show n2)):pb else pb))
       ( _     , BLeq, _      ) -> (EErr  $ "<= takes int, int",e,pb)
       (EInt n1, BGeq, EInt n2) -> (EBool $ n1 >= n2,e, (if db then ((show n1) ++ "≥" ++ (show n2)):pb else pb))
       ( _     , BGeq, _      ) -> (EErr  $ ">= takes int, int",e,pb)
       (EBool b1, BAnd, EBool b2) -> (EBool $ b1 && b2,e, (if db then ((show b1) ++ "and" ++ (show b2)):pb else pb))
       ( _      , BAnd, _      ) -> (EErr  $ "and takes bool, bool",e,pb)
       (EBool b1, BOr , EBool b2) -> (EBool $ b1 || b2,e, (if db then ((show b1) ++ "or" ++ (show b2)):pb else pb))
       ( _      , BOr , _      ) -> (EErr  $ "or takes bool, bool",e,pb)

step (db,dp) pb e (ENot b)
  |not $ value b = let (ex,en,bf) = evaluate (db,dp+1) e b [] in
    (ENot ex,e, if db then (show (ENot b):bf)++pb else pb)
  |otherwise     =
     case b of
       (EBool b1) -> (EBool $ not b1,e, if db then (show (ENot b)):pb else pb)
       _          -> (EErr $ "not takes bool", e, pb)
step (db,dp) pb e (EFst l)
  | not $ value l = let (ex,en,bf) = evaluate (db,dp+1) e l [] in
    (EFst ex, e, if db then ((show l):bf)++pb else pb)
  | otherwise     =
     case l of
      (ELst (x:_)) -> step (db,dp) pb e x
      ENil         -> (ENil, e, if db then (show ENil):pb else pb)
      _            -> (EErr $ "first takes a list", e, pb)
step (db,dp) pb e (ERst l)
  | not $ value l = let (ex,en,bf) = evaluate (db,dp+1) e l [] in
    (ERst ex, e, if db then ((show l):bf)++pb else pb)
  | otherwise     =
     case l of
      (ELst (_:xs)) -> (ELst $ xs, e, if db then (show xs):pb else pb)
      ENil          -> (ENil, e, if db then (show ENil):pb else pb)
      _             -> (EErr $ "rest takes a list", e, pb)
step (db,dp) pb e (EEmt l)
  |not $ value l = let (ex,en,bf) = evaluate (db,dp+1) e l [] in 
    (EEmt ex, e, if db then ((show l):bf)++pb else pb)
  |otherwise     =
    case l of
     ELst [] -> (EBool True, e, if db then (show l):pb else pb)
     ENil -> (EBool True, e, if db then (show ENil):pb else pb)
     _    -> (EBool False, e, pb)
step (db,dp) pb e (ECons v l)
  |not $ value v = let (ex,en,bf) = evaluate (db,dp+1) e v  [] in
    ((ECons ex l), e, if db then ((show (ECons v l)):bf)++pb else pb)
  |not $ value l = let (ex,en,bf) = evaluate (db,dp+1) e l [] in
    ((ECons v ex), e, if db then ((show (ECons v l)):bf)++pb else pb)
  |otherwise     =
    case l of
      (ELst l) -> (ELst $ v:l, e, if db then (show l):pb else pb)
      ENil     -> (ELst $ v: [], e, if db then (show ENil):pb else pb)
      _        -> (EErr "cons takes a value and a list", e, pb)
step (db,_) pb e ENil = (ELst $ [],e, if db then (show ENil):pb else pb )
step (db,dp) pb e (EIf b e1 e2)
  | not $ value b = let (ev,_,eb) = evaluate (db,dp+1) e b (if db then ["if (" ++ (show b) ++ "){" ++ (show e1) ++ "} else {" ++ (show e2) ++ "}"] else []) in
                      step (db,dp) (eb++pb) e (EIf ev e1 e2)
  | otherwise     =
     case b of
      EBool b1 -> evaluate (db,dp+1) e (if b1 then e1 else e2) (if db then ("if (" ++ (show b1) ++ "){" ++ (show e1) ++ "} else {" ++ (show e2) ++ "}"):pb else pb)
      _        -> (EErr "if not given a boolean value", e, pb)
--Applies defined function
step (db,dp) pb e (EApp s lv) =
  case find s e of
   (EFunc f lp e1) -> let (vals,buff) = mEval (db,dp) lv [] [] e in
                        let fenv = (zip lp vals) ++ e in 
                          let (xp, env, buffer) = evaluate (db,dp+1) fenv e1 pb in
                            (xp, env, buffer ++ buff ++ pb)
                          --evaluate d fenv e1 (if d then (buff++f:pb) else pb)
   _               -> (EErr $  "function " ++ s  ++ " is not declared", e, pb)

--call for variable declaration
step (db,dp) pb e (ELet s v)
  | existsIn s e   = (v, findAndReplace s v e, if db then (s ++ " declared as " ++ (show v)):pb else pb)
  | value v        = (v, (s,v):e, if db then (s ++ " declared as " ++ (show v)):pb else pb)
  | otherwise      =
     case v of
      (ELet _ _)    -> (EErr "cannot assign variable to another variable declaration", e, pb)
      (EFunc _ _ _) -> (EErr "cannot assign variable to a function declaration", e, pb)
      _               -> let (ex, en, bf) = evaluate (db, dp+1) e v [] in
                          step (db,dp) bf en (ELet s ex)

step (db,dp) pb e (EPrint exp) =
  let (ex, env,df) = evaluate (db,dp+1) e exp pb in
      (exp, e, ((show $ ex) : pb))
--call for function declaration
step (db,_) pb e (EFunc s l e1)
  | value e1  = (EErr $ "cannot assign function to value", e, pb)
  | existsIn s e = (EStr $ "Function " ++ s ++ " with parameters " ++ (show l), findAndReplace s (EFunc s l e1) e, if db then (s ++ "declared as " ++ (show e1)):pb else pb)
  | otherwise = (EStr $ "Function " ++ s ++ " with parameters " ++ (show l), (s, (EFunc s l e1)):e, pb)

--maps evaluate onto list of expressions
mEval :: DebugEnv -> [Exp] -> [Exp] -> Buffer -> Env -> ([Exp],Buffer)
mEval _ [] sofar sofarBuff  _    = (sofar,sofarBuff)
mEval (db,dp) (x:xs) sofar sofarBuff e  = let (ex,_,b) = evaluate (db,dp+1) e x [] in
                                         mEval (db,dp) xs (ex:sofar) (sofarBuff ++ b) e

existsIn :: String -> Env -> Bool
existsIn _ []   = False
existsIn s ((s1, _):xs)
  | s==s1     = True
  | otherwise = existsIn s xs

find :: String -> Env -> Exp
find s []       = EErr $ "`" ++  s  ++ "` has not been declared"
find s ((s1, v1):xs)
  | s == s1   = v1
  | otherwise = find s xs

findAndReplace :: String -> Exp -> Env -> Env
findAndReplace s v ((s1, v1):xs)
  | s == s1   = (s1, v):xs
  | otherwise = findAndReplace s v xs

fst' :: (a, b, c) -> a
fst' (x, y, z) = x

snd' :: (a, b, c) -> b
snd' (x, y, z) = y

thd :: (a, b, c) -> c
thd (x, y, z) = z

evaluate :: DebugEnv -> Env -> Exp -> Buffer  -> (Exp, Env, Buffer)
evaluate d env exp pb
  | not $ value exp = (\(ex, en, pb) -> evaluate d en ex pb) (step d pb env exp)
  | otherwise     =  (exp, env, pb)
