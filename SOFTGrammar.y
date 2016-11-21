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

Exp   : let var '=' Exp   { ENil }
      | Exp1              { evaluate $1 } 

Exp1  : int               { EInt $1 }
      | str               { EStr $1 }
--    | '(' Exp1 ')'      { Val $2 }
      | true              { EBool True }
      | false             { EBool False } 
      | Exp1 '+' Exp1       { EBinop $1 BAdd $3 }
      | Exp1 '-' Exp1       { EBinop $1 BSub $3 }
      | Exp1 '*' Exp1       { EBinop $1 BMul $3 }
      | Exp1 '/' Exp1       { EBinop $1 BDiv $3 }
      | Exp1 'mod' Exp1     { EBinop $1 BMod $3 }
      | Exp1 '==' Exp1      { EBinop $1 BEql $3 }
      | Exp1 '<' Exp1       { EBinop $1 BLtn $3 }
      | Exp1 '>' Exp1       { EBinop $1 BGtn $3 }
      | Exp1 '>=' Exp1      { EBinop $1 BGeq $3 }
      | Exp1 '<=' Exp1      { EBinop $1 BLeq $3 }
      | Exp1 'and' Exp1   { EBinop $1 BAnd $3 }
      | Exp1 'or' Exp1    { EBinop $1 BOr $3 }
      | 'not' Exp1        { ENot $2 }         
