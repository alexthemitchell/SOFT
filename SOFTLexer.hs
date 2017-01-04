module SOFTLexer where
import Data.Char

data ParseResult a = Ok a | Failed String
type LineNumber = Int
type P a = String -> LineNumber -> ParseResult a

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s ->
  case m s of 
    Ok a -> k a s
    Failed e -> Failed e

returnP :: a -> P a
returnP a = \s -> Ok a

failP :: String -> P a
failP err = \s -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s ->
  case m s of
    Ok a -> Ok a
    Failed e -> k e s

-- Token types --
data Token
      = TokenInt Int
      | TokenFlt Float
      | TokenChar Char
      | TokenVar String
      | TokenStr String
      | TokenFunction
      | TokenNil
      | TokenCons
      | TokenLet
      | TokenTrue
      | TokenFalse
      | TokenEqual
      | TokenDoubleEqual
      | TokenPlus
      | TokenMinus
      | TokenAsterisk
      | TokenMod
      | TokenLT
      | TokenGT
      | TokenLEQ
      | TokenGEQ
      | TokenAnd
      | TokenOr
      | TokenNot
      | TokenIf
      | TokenThen
      | TokenFst
      | TokenEmp
      | TokenRst
      | TokenElse
      | TokenExp
      | TokenFSlash
      | TokenLParen
      | TokenRParen
      | TokenLBrace
      | TokenRBrace
      | TokenEmptyList
      | TokenLSqBrkt
      | TokenRSqBrkt
      | TokenComma
      | TokenPrint
      | TokenEOF
 deriving Show

isNumSymbol :: Char -> Bool
isNumSymbol c = isDigit c || c == '.'

-- Returns need to be changed, leaving broken for now.
-- Lexer --
lexer :: (Token -> P a) -> P a
lexer cont s = 
  case s of 
    []                 -> []
    ('\n':cs)          -> lexer cs
    ('"':cs)           -> lexStr cs
    (c:cs)
       | isSpace c     -> lexer cs
       | isAlpha c     -> lexVar (c:cs)
       | isNumSymbol c -> lexNum (c:cs)
    ('+':cs)           -> TokenPlus : lexer cs
    ('-':cs)           -> TokenMinus : lexer cs
    ('*':cs)           -> TokenAsterisk : lexer cs
    ('/':cs)           -> TokenFSlash : lexer cs
    ('\'':x:'\'':cs)   -> TokenChar x : lexer cs
    ('(':cs)           -> TokenLParen : lexer cs
    (')':cs)           -> TokenRParen : lexer cs
    ('{':cs)           -> TokenLBrace : lexer cs
    ('}':cs)           -> TokenRBrace : lexer cs
    ('[':cs)           -> TokenLSqBrkt : lexer cs
    (']':cs)           -> TokenRSqBrkt : lexer cs
    (':':cs)           -> TokenCons : lexer cs
    ('<':'=':cs)       -> TokenLEQ : lexer cs
    ('>':'=':cs)       -> TokenGEQ : lexer cs
    ('=':'=':cs)       -> TokenDoubleEqual : lexer cs
    ('<':cs)           -> TokenLT : lexer cs
    ('>':cs)           -> TokenGT : lexer cs
    ('=':cs)           -> TokenEqual : lexer cs
    (',':cs)           -> TokenComma : lexer cs

lexStr cs = TokenStr str : if length rest /= 0 then lexer (tail rest) else lexer rest
  where (str, rest) = span (\x -> x /= '"') cs

lexNum cs
  | any (=='.') num   =  TokenFlt (read $ '0': num) : lexer rest
  | otherwise         =  TokenInt (read num)        : lexer rest
  where (num,rest)    =  span isNumSymbol cs


lexVar cs =
  case span (\x->isAlpha x || isNumSymbol x) cs of
      ("nil",rest)    -> TokenNil         : lexer rest
      (":",rest)      -> TokenCons        : lexer rest
      ("let",rest)    -> TokenLet         : lexer rest
      ("true",rest)   -> TokenTrue        : lexer rest
      ("false", rest) -> TokenFalse       : lexer rest
      ("mod", rest)   -> TokenMod         : lexer rest
      ("and", rest)   -> TokenAnd         : lexer rest
      ("or", rest)    -> TokenOr          : lexer rest
      ("not", rest)   -> TokenNot         : lexer rest
      ("if", rest)    -> TokenIf          : lexer rest
      ("then", rest)  -> TokenThen        : lexer rest
      ("else", rest)  -> TokenElse        : lexer rest
      ("first", rest) -> TokenFst         : lexer rest
      ("empty", rest) -> TokenEmp         : lexer rest
      ("rest" , rest) -> TokenRst         : lexer rest
      ("function", rest) -> TokenFunction : lexer rest
      ("print", rest) -> TokenPrint       : lexer rest
      ("EOF", rest)   -> TokenEOF         : lexer rest
      (var,rest)      -> TokenVar var     : lexer rest
