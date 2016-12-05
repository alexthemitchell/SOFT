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
      | TokenComment
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

-- Lexer --
lexer :: String -> [Token]
lexer []               = []
lexer ('\n':cs)        = [TokenNewline]
lexer ('#':cs)         = [TokenComment]
lexer ('"':cs)         = lexStr cs
lexer (c:cs)
      | isSpace c      = lexer cs
      | isAlpha c      = lexVar (c:cs)
      | isDigit c || c =='.'      = lexNum (c:cs)
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

lexStr cs = TokenStr str : if length rest /= 0 then lexer (tail rest) else lexer rest
  where (str, rest) = span (\x -> x /= '"') cs

lexNum cs = if any (=='.') num then TokenFlt (read $ "0" ++ num) : lexer rest else TokenInt (read num) : lexer rest
    where (num,rest) = span (\x-> isDigit x || x =='.') cs

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
