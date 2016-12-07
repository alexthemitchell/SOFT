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
  '-'       { TokenMinus }
  '*'       { TokenAsterisk }
  'mod'     { TokenMod }
  '<'       { TokenLT }
  '>'       { TokenGT }
  '<='      { TokenLEQ }
  '>='      { TokenGEQ }
  'and'     { TokenAnd }
  'or'      { TokenOr  }
  'not'     { TokenNot }
  if        { TokenIf  }
  then      { TokenThen }
  else      { TokenElse }
  first     { TokenFst }
  rest      { TokenRst }
  empty     { TokenEmp }
  '/'       { TokenFSlash }
  '('       { TokenLParen }
  ')'       { TokenRParen }
  '{'       { TokenLBrace }
  '}'       { TokenRBrace }
  '['       { TokenLSqBrkt }
  ']'       { TokenRSqBrkt }
  int       { TokenInt $$ }
  float     { TokenFlt $$ }
  char      { TokenChar $$ }
  ','       { TokenComma }
  str       { TokenStr $$ }
  '\n'      { TokenNewline }
%%
Cmd     : Exp         { $1 }
        | {- Empty -} { ENil }
        | '#'         { ENil }

Exp     : let var '=' Closure                         { ELet $2 $4 }
        | function var '(' Parameters ')' '{' Exp '}' { EFunc $2 (reverse $4) $7} 
        | Closure                                     { $1 }
        | if Exp then Exp else Exp                    { EIf $2 $4 $6 }
        | first Exp                                   { EFst $2 }
        | rest Exp                                    { ERst $2 }
        | empty Exp                                   { EEmt $2 }

Closure : '(' Exp ')'       { $2 }
        | List              { $1 } 
        | BOpNum            { $1 }

List : '[' ListLiteral ']' { ELst $ reverse $2 } -- (2 of 2) ... so we must reverse the input here.
     | Closure ':' List    { ECons $1 $3 }

ListLiteral : ListLiteral ',' Exp    { $3 : $1 } -- (1 of 2) We use left recursion for stack overflow reasons... ^^
            | Exp                    { [$1] }
            | {- Empty -}            { [] }

Parameters : Parameters ',' var  { $3 : $1 }
           | var                 { [$1] }
           | {- empty -}         { [] }

BOpNum  : Exp '+' Exp      { EBinop $1 BAdd $3 }
        | Exp '-' '-' Exp  { EBinop $1 BAdd $4 }
        | Exp '-' Exp      { EBinop $1 BSub $3 }
        | Exp '*' Exp      { EBinop $1 BMul $3 }
        | Exp '/' Exp      { EBinop $1 BDiv $3 }
        | Exp 'mod' Exp    { EBinop $1 BMod $3 }
        | Exp '==' Exp     { EBinop $1 BEql $3 }
        | Exp '<' Exp      { EBinop $1 BLtn $3 }
        | Exp '>' Exp      { EBinop $1 BGtn $3 }
        | Exp '>=' Exp     { EBinop $1 BGeq $3 }
        | Exp '<=' Exp      { EBinop $1 BLeq $3 }
        | Value              { $1 }
        | BOpBool            { $1 }

BOpBool : Exp 'and' Exp     { EBinop $1 BAnd $3 }
        | Exp 'or' Exp      { EBinop $1 BOr $3 }
        |'not' Exp          { ENot $2 }
        | Value               { $1 }


Value   : int               { EInt $1 }
        | float             { EFlt $1 }
        | char              { EChar $1 }
        | var '(' ListLiteral ')' {EApp $1 $3}
        | var               { EVar $1 }
        | Bool              { $1 }
        | str               { EStr $1 }
        | nil               { ENil }

Bool    : true              { EBool True }
        | false             { EBool False }
