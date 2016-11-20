{
-- Output from grammar.y
--  DO NOT MAKE CHANGES IN THIS FILE
--  Instead, edit grammar.y and run:
--  happy grammar.y
}

%name parse
%tokentype { Token }
%error { parseError }

%token
  let   { TokenLet }
  var   { TokenVar $$ }
  true  { TokenTrue }
  false { TokenFalse }
  '#'   { TokenComment }
  '='   { TokenEqual }
  '+'   { TokenPlus } 
  '-'   { TokenMinus}
  '*'   { TokenAsterisk }
  '/'   { TokenFSlash }
  '('   { TokenLParen }
  ')'   { TokenRParen }
  '{'   { TokenLBrace }
  '}'   { TokenRBrace }
  '['   { TokenLSqBrkt }
  ']'   { TokenRSqBrkt }
  '"'   { TokenQuotation }

%% 

Exp : let var '=' Exp { Let $2 $4 }
    | Exp1            { Exp1 $1 }

Exp1 : Exp1 '+' Term  { Plus $1 $3 }
     | Exp1 '-' Term  { Minus $1 $3 }
     | Term           { Term $1 }

Term : Exp1 '*' Term  { Times $1 $3 }
     | Exp1 '/' Term  { Div $1 $3 }
     | Factor         { Factor $1 }

Factor : int          { Int $1 }
       | var          { Var $1 }
       | '(' Exp ')'  { Brack $2 }

{
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

}
