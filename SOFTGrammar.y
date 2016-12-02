{
-- Output from SOFTGrammar.y
--  DO NOT MAKE CHANGES IN THIS FILE
--  Instead, edit SOFTGrammar.y and run:
--  happy SOFTGrammar.y
module SOFTGrammar where
import SOFTLexer
import SOFTEval
}

%monad { E } { thenE } { returnE }

%name parse
%tokentype { SOFTLexer.Token }
%error { parseError }

%token
  nil       { TokenNil }
  ':'       { TokenCons }
  let       { TokenLet }
  function  { TokenFunction }
  var       { TokenVar $$ }
  true      { TokenTrue }
  false     { TokenFalse }
  '#'       { TokenComment }
  '='       { TokenEqual }
  '=='      { TokenDoubleEqual }
  '+'       { TokenPlus } 
  '-'       { TokenMinus}
  '*'       { TokenAsterisk }
  'mod'     { TokenMod }
  '<'       { TokenLT }
  '>'       { TokenGT }
  '<='      { TokenLEQ }
  '>='      { TokenGEQ }
  'and'     { TokenAnd }
  'or'      { TokenOr  }
  'not'     { TokenNot }
  '/'       { TokenFSlash }
  '('       { TokenLParen }
  ')'       { TokenRParen }
  '{'       { TokenLBrace }
  '}'       { TokenRBrace }
  '['       { TokenLSqBrkt }
  ']'       { TokenRSqBrkt }
  int       { TokenInt $$ }
  char      { TokenChar $$ }
  ','       { TokenComma }
  str       { TokenStr $$ }
%% 

Exp     : let var '=' Closure                         { evaluate $ ELet $2 $ evaluate $4 }
        | function var '(' Parameters ')' '{' Exp '}' { evaluate $ EFunc $2 (reverse $4) $7 }
        | var                                         { evaluate $ EVar $1 }
        | Value ':' '[' List ']'                      { ELst $ $1 : (reverse $ $4) } 
        | Closure                                     { evaluate $1 }
        | '#' Exp                                     { ENil }

Closure : '(' Exp ')'       { evaluate $2 }
        | '[' List ']'      { ELst $ reverse $2 } -- (2 of 2) ... so we must reverse the input here. 
        | BOpNum            { evaluate $1 } 

List    : List ',' Value    { $3 : $1 } -- (1 of 2) We use left recursion for stack overflow reasons... ^^
        | List ','          { $1 }
        | Value             { [$1] }
        | {- empty -}       { [] }

Parameters : Parameters ',' var  { $3 : $1 }
           | var                 { [$1] }
           | {- empty -}         { [] }

BOpNum  : Value '+' Value   { EBinop $1 BAdd $3 }
        | Value '-' Value   { EBinop $1 BSub $3 }
        | Value '*' Value   { EBinop $1 BMul $3 }
        | Value '/' Value   { EBinop $1 BDiv $3 }
        | Value 'mod' Value { EBinop $1 BMod $3 }
        | Value '==' Value  { EBinop $1 BEql $3 }
        | Value '<' Value   { EBinop $1 BLtn $3 }
        | Value '>' Value   { EBinop $1 BGtn $3 }
        | Value '>=' Value  { EBinop $1 BGeq $3 }
        | Value '<=' Value  { EBinop $1 BLeq $3 }
        | BOpBool           { evaluate $1 }

BOpBool : Value '==' Value  { EBinop $1 BEql $3 }
        | Value 'and' Value { EBinop $1 BAnd $3 }
        | Value 'or' Value  { EBinop $1 BOr $3 }
        |'not' Value        { ENot $2 }
        | Value             { evaluate $1 } 

 
Value   : int               { EInt $1 }
        | char              { EChar $1 }
        | Bool              { evaluate $1 }
        | str               { EStr $1 }
        | nil               { ENil }

Bool    : true              { EBool True }
        | false             { EBool False } 
