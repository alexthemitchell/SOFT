import Prelude
import SOFTGrammar
import SOFTLexer
import SOFTEval
import System.IO
import System.Environment

--{
--REPL code taken from:
--https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours/Building_a_REPL
--}
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
        then return ()
        else action result >> until_ pred prompt action

main :: IO ()
main = do args <- getArgs
          case length args of
            1 -> do
              code <- readFile $ args !! 0
              let loc = lines code
              runCode loc
            otherwise -> runRepl

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: String -> IO ()
evalAndPrint input = print . parse . lexer $ input

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt ">> ") evalAndPrint

runCode :: [String] -> IO ()
runCode l = do 
  let tokenizedInput  = map lexer l
  print $ runCodeKernel [] tokenizedInput

runCodeKernel :: Env -> [[Token]] -> Exp
runCodeKernel e [x] = do
  let monad = parse x
  case monad of
    (Ok m) -> fst $ step e m
    (Failed s) -> EErr s
runCodeKernel e (x:xs) = do
  let monad = parse x
  case monad of 
    (Ok m) -> (\(_, env) -> runCodeKernel env xs) $ step e m 
    (Failed s) -> EErr s
 
