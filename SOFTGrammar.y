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
  char  { TokenChar $$ }
  ','   { TokenComma }
%% 

Exp     : let var '=' Exp   { ENil }
        | Closure           { evaluate $1 }

Closure : '(' Exp ')'       { evaluate $2 }
        | BOpNum            { evaluate $1 } 

BOpNum  : Value '+' Value     { EBinop $1 BAdd $3 }
        | Value '-' Value     { EBinop $1 BSub $3 }
        | Value '*' Value     { EBinop $1 BMul $3 }
        | Value '/' Value     { EBinop $1 BDiv $3 }
        | Value 'mod' Value   { EBinop $1 BMod $3 }
        | Value '==' Value    { EBinop $1 BEql $3 }
        | Value '<' Value     { EBinop $1 BLtn $3 }
        | Value '>' Value     { EBinop $1 BGtn $3 }
        | Value '>=' Value    { EBinop $1 BGeq $3 }
        | Value '<=' Value    { EBinop $1 BLeq $3 }
        | BOpBool           { evaluate $1 }

BOpBool : Value '==' Value    { EBinop $1 BEql $3 }
        | Value 'and' Value   { EBinop $1 BAnd $3 }
        | Value 'or' Value    { EBinop $1 BOr $3 }
        |'not' Value         { ENot $2 }
        | Value           { evaluate $1 } 

Value   : int               { EInt $1 }
        | char              { EChar $1 }
        | Bool           { evaluate $1 }

Bool    : true              { EBool True }
        | false             { EBool False } 