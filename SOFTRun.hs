import Prelude
import SOFTGrammar
import SOFTLexer
import SOFTEval
import System.IO
import System.Environment

--{
--REPL code adapted from:
--https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Building_a_REPL
--}
until_ :: Env -> (String -> Bool) -> IO String -> IO ()
until_ env pred prompt = do
    r <- prompt
    let result = stripComments r
    if pred result
        then return ()
    else if result == ":env" then do
        putStrLn $ show env
        until_ env pred prompt
    else if (take 8 result == ":explain")  then do
        let input =  (\(':':'e':'x':'p':'l':'a':'i':'n':xs) -> xs) result
        let monad = parse.lexer $ stripComments $ input
        case monad of
          (Ok m) -> do
            let (ex,en,pb) = evaluate True env m []
            printAll (reverse pb)
            putStrLn $ show ex
            until_ en pred prompt
          (Failed s) -> do
            putStrLn s
            until_ env pred prompt
    else do
          let monad = parse .lexer $ result
          case monad of
            (Ok m) -> do
              let (ex, en, pb) = evaluate False env m []
              printAll pb
              putStrLn (show ex)
              until_ en pred prompt
            (Failed s) -> do
              putStrLn s
              until_ env pred prompt

printAll :: [String] -> IO ()
printAll [] =  putStr ""
printAll (x:xs) = do
  putStrLn $ show x
  printAll xs

stripComments :: String -> String
stripComments []       = []
stripComments ('#':xs) = stripComments rest
  where (line,rest) = span (/= '\n') xs
stripComments (x:xs)   = x:stripComments xs

splitProgram :: String -> [String] -> [String]
splitProgram [] sofar = sofar
splitProgram program ("":xs) = splitProgram program xs
splitProgram ('f':'u':'n':'c':'t':'i':'o':'n':xs) sofar =
    splitProgram extra (sofar ++ ["function" ++ functionName ++ functionBody])
    where (functionName,rest) = span (/='{') xs
          (functionBody,extra)= matchDelim rest '{' '}' 0 ("","")
splitProgram('(':xs) sofar =  
    let (line,rest) = matchDelim ('(':xs) '(' ')' 0 ("","") in
    splitProgram rest (sofar ++ [line])
splitProgram ('\n':xs) sofar = splitProgram xs sofar
splitProgram s sofar = 
    let (line,rest) = span (/= '\n') s in
    splitProgram rest (sofar ++ [line])


--Precondition: the first character of the program must be the deliminator
matchDelim  :: String -> Char -> Char -> Int -> (String,String) -> (String,String)
matchDelim (x:xs) o c 0 (before, _ ) = if before == [] then matchDelim xs o c 1 (x:"","") else (before, xs)
matchDelim [] _ _ _ _      = ("you","mismatched")
matchDelim (x:xs) open close stck (before,_)
  | x == open  = matchDelim xs open close (stck+1) (before++[x],"")
  | x == close = matchDelim xs open close (stck-1) (before++[x],"")
  | otherwise  = matchDelim xs open close stck (before++[x], "")

main :: IO ()
main = do args <- getArgs
          case length args of
            1 -> do
              code <- readFile $ args !! 0
              let noCom = stripComments code
              let loc = splitProgram noCom [[]]
              runCode loc
            otherwise -> runRepl

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: String -> IO ()
evalAndPrint input = print . parse . lexer $ input

runRepl :: IO ()
runRepl = until_ [] (== ":quit") (readPrompt ">> ")


runCode :: [String] -> IO ()
runCode l =
    let tokenizedInput  = map lexer l in
    runCodeKernel [] tokenizedInput []

runCodeKernel :: Env -> [[Token]] -> Buffer  -> IO()
runCodeKernel e [x] pb =
  let monad = parse x in
  case monad of
    (Ok m) -> do
      let (exp, env, pb) = evaluate False e m []
      printAll pb
    (Failed s) -> putStrLn $ show $ EErr s
runCodeKernel e (x:xs) pb =
  let monad = parse x in
  case monad of
    (Ok m) ->
      let (_,env,pb') = evaluate False e m [] in
      runCodeKernel env xs (pb ++ pb')
    (Failed s) -> putStrLn $ show $  EErr s

