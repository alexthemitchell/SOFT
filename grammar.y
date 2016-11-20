{
-- Output from grammar.y
--  DO NOT MAKE CHANGES IN THIS FILE
--  Instead, edit grammar.y and run
--  happy grammar.y
}

%name parse
%tokentype {Token}
%error {parseError}

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

