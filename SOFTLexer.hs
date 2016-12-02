module SOFTLexer where
import Data.Char

-- Token types --
data Token
      = TokenInt Int
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
      | TokenFSlash
      | TokenLParen
      | TokenRParen
      | TokenLBrace
      | TokenRBrace
      | TokenLSqBrkt
      | TokenRSqBrkt
      | TokenComma
 deriving Show

-- Lexer --
lexer :: String -> [Token]
lexer [] = []
lexer ('#':cs)         = TokenComment : lexer cs
lexer ('"':cs)         = lexStr cs
lexer (c:cs) 
      | isSpace c      = lexer cs
      | isAlpha c      = lexVar (c:cs)
      | isDigit c      = lexNum (c:cs)
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

lexNum cs = TokenInt (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexVar cs =
   case span isAlpha cs of
      ("nil",rest)    -> TokenNil : lexer rest
      (":",rest)   -> TokenCons : lexer rest
      ("let",rest)    -> TokenLet : lexer rest
      ("true",rest)   -> TokenTrue : lexer rest
      ("false", rest) -> TokenFalse : lexer rest
      ("mod", rest)   -> TokenMod : lexer rest
      ("and", rest)   -> TokenAnd : lexer rest
      ("or", rest)    -> TokenOr : lexer rest
      ("not", rest)   -> TokenNot : lexer rest
      ("function", rest) -> TokenFunction : lexer rest
      (var,rest)      -> TokenVar var : lexer rest
