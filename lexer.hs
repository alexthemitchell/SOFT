-- Error handling --
parseError :: [Token] -> a
parseError _ = error "Parse Error lol"    

-- Token types --
data Token
      = TokenLet
      | TokenVar
      | TokenTrue
      | TokenFalse
      | TokenComment
      | TokenEqual
      | TokenPlus
      | TokenMinus
      | TokenAsterisk
      | TokenFSlash
      | TokenLParen
      | TokenRParen
      | TokenLBrace
      | TokenRBrace
      | TokenLSqBrkt
      | TokenRSqBrkt
      | TokenQuotation
 deriving Show

-- Lexer --
lexer :: String -> [Token]
lexer [] = []
lexer (c:cs) 
      | isSpace c = lexer cs
      | isAlpha c = lexVar (c:cs)
      | isDigit c = lexNum (c:cs)
lexer ('#':cs) = TokenComment : lexer cs
lexer ('=':cs) = TokenEqual : lexer cs
lexer ('+':cs) = TokenPlus : lexer cs
lexer ('-':cs) = TokenMinus : lexer cs
lexer ('*':cs) = TokenAsterisk : lexer cs
lexer ('/':cs) = TokenFSlash : lexer cs
lexer ('(':cs) = TokenLParen : lexer cs
lexer (')':cs) = TokenRParen : lexer cs
lexer ('{':cs) = TokenLBrace : lexer cs
lexer ('}':cs) = TokenRBrace : lexer cs
lexer ('[':cs) = TokenLSqBrkt : lexer cs
lexer (']':cs) = TokenLSqBrkt : lexer cs
lexer ('"':cs) = TokenQuotation : lexer cs


lexNum cs = TokenInt (read num) : lexer rest
      where (num,rest) = span isDigit cs

lexVar cs =
   case span isAlpha cs of
      ("let",rest) -> TokenLet : lexer rest
      ("true",rest) -> TokenTrue : lexer rest
      ("false", rest) -> TokenFalse : lexer rest
      (var,rest)   -> TokenVar var : lexer rest
