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

Exp1  : Val               { Val $1}
      | Op                { Op $1 }
--    | '(' Exp1 ')'      { Val $2 }

Val   : Num               { Num $1 }
      | str               { EStr $1 }
      | Bool              { Bool $1 }

Bool   : true             { EBool True }
      | false             { EBool False } 

Num   : int               { EInt $1 }

Op    : Num '+' Num       { BAdd $1 $3 }
      | Num '-' Num       { BSub $1 $3 }
      | Num '*' Num       { BMul $1 $3 }
      | Num '/' Num       { BDiv $1 $3 }
      | Num 'mod' Num     { BMod $1 $3 }
      | Val '==' Val      { BEql $1 $3 }
      | Num '<' Num       { BLtn $1 $3 }
      | Num '>' Num       { BGtn $1 $3 }
      | Num '>=' Num      { BGeq $1 $3 }
      | Num '<=' Num      { BLeq $1 $3 }
      | Bool 'and' Bool   { BAnd $1 $3 }
      | Bool 'or' Bool    { BOr $1 $3 }
      | 'not' Bool        { ENot $2 }
