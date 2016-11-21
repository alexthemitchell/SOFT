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
  '=='  { TokenDoubleEqual }
  '+'   { TokenPlus } 
  '-'   { TokenMinus}
  '*'   { TokenAsterisk }
  'mod' { TokenMod }
  '<'   { TokenLT }
  '>'   { TokenGT }
  '<='  { TokenLEQ }
  '>='  { TokenGEQ }
  'and' { TokenAnd }
  'or'  { TokenOr  }
  'not' { TokenNot }
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

Exp1 : int            { EInt $1 }
     | true           { True }
     | false          { False }
     | str            { EStr $1 }
     | Exp1 '+' Exp1  { EAdd $1 $3 }
     | Exp1 '-' Exp1  { ESub $1 $3 }
     | Exp1 '*' Exp1  { EMul $1 $3 }
     | Exp1 '/' Exp1  { EDiv $1 $3 }
     | Exp1 'mod' Exp1  { EMod $1 $3 }
     | Exp1 '==' Exp1 { EEql $1 $3 }
     | Exp1 '<' Exp1  { ELtn $1 $3 }
     | Exp1 '>' Exp1  { EGtn $1 $3 }
     | Exp1 '>=' Exp1 { EGeq $1 $3 }
     | Exp1 '<=' Exp1 { ELeq $1 $3 }
     | Exp1 'and' Exp1{ EAnd $1 $3 }
     | Exp1 'or' Exp1 { EAnd $1 $3 }
     | 'not' Exp1     { ENot $2 }
     | '(' Exp ')'    { EClos $2 }


