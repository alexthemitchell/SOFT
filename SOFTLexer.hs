module SOFTLexer where
import Data.Char

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
      | TokenFSlash
      | TokenLParen
      | TokenRParen
      | TokenLBrace
      | TokenRBrace
      | TokenEmptyList
      | TokenLSqBrkt
      | TokenRSqBrkt
      | TokenComma
      | TokenNewline
 deriving Show

--Helper functions for parsing numbers--
isNumSymbol :: Char -> Bool
isNumSymbol c = isDigit c || c == '.' || c == '-'

fixFloat :: String -> String
fixFloat ('.':xs) = '0':'.':xs
fixFloat ('-':'.':xs) = '-':'0':'.':xs
fixFloat str = str


-- Lexer --
lexer :: String -> [Token]
lexer []               = []
lexer ('\n':cs)        = [TokenNewline]
lexer ('#':cs)         = lexComment cs
lexer ('"':cs)         = lexStr cs
lexer (' ':'-':cs)     = TokenMinus : lexer cs
lexer (c:cs)
      | isSpace c      = lexer cs
      | isAlpha c      = lexVar (c:cs)
      | isNumSymbol c  = lexNum (c:cs)
lexer ('+':cs)         = TokenPlus : lexer cs
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

lexComment :: String -> [Token] 
lexComment [] = []
lexComment ('\n':cs) = lexer cs
lexComment (c:cs) = lexComment cs

lexStr cs = TokenStr str : if length rest /= 0 then lexer (tail rest) else lexer rest
  where (str, rest) = span (\x -> x /= '"') cs

lexNum cs
  | any (=='.') num   =  TokenFlt (read $ fixFloat num) : lexer rest
  | otherwise         =  TokenInt (read num) : lexer rest
  where (num,rest)    =  span isNumSymbol cs


lexVar cs =
   case span isAlpha cs of
      ("nil",rest)    -> TokenNil : lexer rest
      (":",rest)      -> TokenCons : lexer rest
      ("let",rest)    -> TokenLet : lexer rest
      ("true",rest)   -> TokenTrue : lexer rest
      ("false", rest) -> TokenFalse : lexer rest
      ("mod", rest)   -> TokenMod : lexer rest
      ("and", rest)   -> TokenAnd : lexer rest
      ("or", rest)    -> TokenOr : lexer rest
      ("not", rest)   -> TokenNot : lexer rest
      ("if", rest)    -> TokenIf : lexer rest
      ("then", rest)  -> TokenThen : lexer rest
      ("else", rest)  -> TokenElse : lexer rest
      ("first", rest) -> TokenFst  : lexer rest
      ("empty", rest) -> TokenEmp  : lexer rest
      ("rest" , rest) -> TokenRst  : lexer rest
      ("function", rest) -> TokenFunction : lexer rest
      (var,rest)      -> TokenVar var : lexer rest
