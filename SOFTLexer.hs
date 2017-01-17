module SOFTLexer where
import Data.Char

-- Error Monad
data ParseResult a = Ok a | Failed String
type LineNumber = Int
type P a = String -> LineNumber -> ParseResult a

getLineNo :: P LineNumber
getLineNo = \s l -> Ok l

thenP :: P a -> (a -> P b) -> P b
m `thenP` k = \s l ->
  case m s l of
    Ok a -> k a s l
    Failed e -> Failed e

returnP :: a -> P a
returnP a = \s l -> Ok a

failP :: String -> P a
failP err = \s l -> Failed err

catchP :: P a -> (String -> P a) -> P a
catchP m k = \s l ->
  case m s l of
    Ok a -> Ok a
    Failed e -> k e s l

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

-- Lexer --
-- current implementation contains type errors, don't properly make recursive call.
lexer :: (Token -> P a) -> P a
lexer cont s =
  case s of
    []     -> \line -> cont TokenEOF [] line
    '\n':cs -> \line -> lexer cont cs (line + 1)
    --'"':cs -> lexer won't work yet bc of lexStr
    '+':cs -> \line -> cont TokenPlus cs line
    '-':cs -> \line -> cont TokenMinus cs line
    {--
    '+':cs -> \line -> lexer cont TokenPlus cs line
    '+':cs -> \line -> lexer cont TokenPlus cs line
    '+':cs -> \line -> lexer cont TokenPlus cs line
    '+':cs -> \line -> lexer cont TokenPlus cs line
--}

{--
-- everything in here will need to be revised
lexer []               = []
lexer ('\n':cs)        = lexer cs
lexer ('"':cs)         = lexStr cs
lexer (c:cs)
      | isSpace c      = lexer cs
      | isAlpha c      = lexVar (c:cs)
      | isNumSymbol c  = lexNum (c:cs)
lexer ('+':cs)         = TokenPlus : lexer cs
lexer ('-':cs)         = TokenMinus : lexer cs
lexer ('*':cs)         = TokenAsterisk : lexer cs
lexer ('/':cs)         = TokenFSlash : lexer cs
lexer ('\'':x:'\'':cs) = TokenChar x : lexer cs
lexer ('(':cs)         = TokenLParen : lexer cs
lexer (')':cs)         = TokenRParen : lexer cs
lexer ('{':cs)         = TokenLBrace : lexer cs
lexer ('}':cs)         = TokenRBrace : lexer cs
lexer ('[':cs)         = TokenLSqBrkt : lexer cs
lexer (']':cs)         = TokenRSqBrkt : lexer cs
lexer (':':cs)         = TokenCons : lexer cs
lexer ('<':'=':cs)     = TokenLEQ : lexer cs
lexer ('>':'=':cs)     = TokenGEQ : lexer cs
lexer ('=':'=':cs)     = TokenDoubleEqual : lexer cs
lexer ('<':cs)         = TokenLT : lexer cs
lexer ('>':cs)         = TokenGT : lexer cs
lexer ('=':cs)         = TokenEqual : lexer cs
lexer (',':cs)         = TokenComma : lexer cs
>>>>>>> df5d9ab28e75125c81394483fc3b12e661eedd98

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
--}
