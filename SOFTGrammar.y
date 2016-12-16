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
  print     { TokenPrint }

%nonassoc '>' '<' '<=' '>=' '==' 'and' 'or' 'mod'
%left '+' '-'
%left '*' '/'
%%

Exp     : let var '=' Closure                         { ELet $2 $4 }
        | function var '(' Parameters ')' '{' Closure '}' { EFunc $2 (reverse $4) $7} 
        | Closure                                     { $1 }
        | print Closure                                   {EPrint $2}
        
Closure : '(' Closure ')'       { $2 }
        | List                  { $1 } 
        | if '(' Bool ')' '{' Exp '}' else '{' Exp '}' { EIf $3 $6 $10 }
        | Value                 {$1 }
        | {- Empty -}           {ENil}

List : '[' ListLiteral ']' { ELst $ reverse $2 } -- (2 of 2) ... so we must reverse the input here.
     | Closure ':' List    { ECons $1 $3 }

ListLiteral : ListLiteral ',' Closure    { $3 : $1 } -- (1 of 2) We use left recursion for stack overflow reasons... ^^
            | Closure                    { [$1] }
            | {- Empty -}            { [] }

Parameters : Parameters ',' var  { $3 : $1 }
           | var                 { [$1] }
           | {- empty -}         { [] }

BOpNum  : Closure '+' Closure      { EBinop $1 BAdd $3 }
        | Closure '-' Closure      { EBinop $1 BSub $3 }
        | Closure '*' Closure      { EBinop $1 BMul $3 }
        | Closure '/' Closure      { EBinop $1 BDiv $3 }
        | Closure 'mod' int    { EBinop $1 BMod $ EInt $3 }

Value   : '-' int           { EInt $ negate $2 }
        | '-' float         { EFlt $ negate $2 }
       	| int               { EInt $1 }
        | float             { EFlt $1 }
        | char              { EChar $1 }
        | var '(' ListLiteral ')' {EApp $1 $3}
        | var               { EVar $1 }
        | Bool              { $1 }
        | str               { EStr $1 }
        | nil               { ENil }
        | first List        { EFst $2 }
        | rest List         { ERst $2 }
        | BOpNum            { $1 }

Bool    : true              { EBool True }
        | false             { EBool False }
        | 'not' Bool          { ENot $2 }
        | Bool 'and' Bool   { EBinop $1 BAnd $3 }
        | Bool 'or' Bool    { EBinop $1 BOr $3 }
        | Closure '==' Closure  { EBinop $1 BEql $3 }
        | Closure '<' Closure      { EBinop $1 BLtn $3 }
        | Closure '>' Closure { EBinop $1 BGtn $3 }
        | Closure '>=' Closure { EBinop $1 BGeq $3 }
        | Closure '<=' Closure { EBinop $1 BLeq $3 }
        | empty List        { EEmt $2 }
