{
-- Output from SOFTGrammar.y
--  DO NOT MAKE CHANGES IN THIS FILE
--  Instead, edit SOFTGrammar.y and run:
--  happy SOFTGrammar.y
module SOFTGrammar where
import SOFTLexer
import SOFTEval
}

%name parse
%tokentype { SOFTLexer.Token }
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
  int   { TokenInt $$ }
  str   { TokenStr $$ }
%% 

Exp : let var '=' Exp { ENil }
    | Exp1            { evaluate $1 } 

Exp1 : Exp1 '+' Exp1  { EAdd $1 $3 }
     | Exp1 '-' Exp1  { ESub $1 $3 }
     | Exp1 '*' Exp1  { EMul $1 $3 }
     | Exp1 '/' Exp1  { EDiv $1 $3 }
     | int            { EInt $1 }
     | str            { EStr $1 }
     | '(' Exp ')'    { EClos $2 }
