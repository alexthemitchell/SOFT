import Prelude 
import SOFTGrammar
import SOFTLexer
import System.IO
import System.Environment

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do 
    result <- prompt
    if pred result 
        then return ()
        else action result >> until_ pred prompt action

main :: IO ()
main = do args <- getArgs
          case length args of
            1 -> evalAndPrint $ args !! 0
            otherwise -> runRepl

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalAndPrint :: String -> IO ()
evalAndPrint input = print . parse . lexer $ input

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt ">> ") evalAndPrint



