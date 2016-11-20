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
parseError :: [Token] -> a
parseError _ = error "Parse Error lol"
}
