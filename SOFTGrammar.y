{
-- Output from SOFTGrammar.y
--  DO NOT MAKE CHANGES IN THIS FILE
--  Instead, edit SOFTGrammar.y and run:
--  happy SOFTGrammar.y
module SOFTGrammar where
import SOFTLexer
import SOFTEval
}

%monad { P } { thenP } { returnP }
%lexer { lexer } { TokenEOF }

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
        | print Closure                                   {EPrint $2}
        | Closure                                     { $1 }
        
Closure : '(' Closure ')'       { $2 }
        | List                  { $1 } 
        | var                   { EVar $1 }
        | if '(' Bool ')' '{' Exp '}' else '{' Exp '}' { EIf $3 $6 $10 }
        | Value                 {$1 }

List : '[' ListLiteral ']' { ELst $ reverse $2 } -- (2 of 2) ... so we must reverse the input here.
     | Closure ':' List   { ECons $1 $3 }
     | var                { EVar $1 }
     | rest '(' List ')'  { ERst $3 }
     | rest '(' var ')'   { ERst $ EVar $3 }

ListLiteral : ListLiteral ',' Closure    { $3 : $1 } -- (1 of 2) We use left recursion for stack overflow reasons... ^^
            | Closure                    { [$1] }
            | List                       { [$1] }
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
        | first '(' List ')'{ EFst $3 }
        | first '(' var ')' { EFst $ EVar $3 }
        | BOpNum            { $1 }

Bool    : true              { EBool True }
        | false             { EBool False }
        | var               { EVar $1}
        | 'not' '(' Bool ')'{ ENot $3 }
        | '(' Closure 'and' Closure ')'   { EBinop $2 BAnd $4 }
        | '(' Closure 'or' Closure ')'    { EBinop $2 BOr $4 }
        | '('Closure '==' Closure  ')' { EBinop $2 BEql $4 }
        | '('Closure '<' Closure ')'     { EBinop $2 BLtn $4 }
        | '('Closure '>' Closure ')' { EBinop $2 BGtn $4 }
        | '('Closure '>=' Closure ')' { EBinop $2 BGeq $4 }
        | '('Closure '<=' Closure ')' { EBinop $2 BLeq $4 }
        | empty '(' List ')'        { EEmt $3 }
        | empty '(' var ')'        { EEmt $ EVar $3} 
