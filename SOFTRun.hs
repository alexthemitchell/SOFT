import Prelude
import SOFTGrammar
import SOFTLexer
import SOFTEval
import System.IO
import System.Environment
import System.Directory
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
    else if result == ""
        then until_ env pred prompt
    else if result == ":env" then do
        putStrLn $ show env
        until_ env pred prompt
    else if (take 8 result == ":explain")  then do
      let input =  (\(':':'e':'x':'p':'l':'a':'i':'n':xs) -> xs) result
      let monad = parse.lexer $ stripComments $ input
      case monad of
        (Ok m) -> do
          let (ex,en,pb) = evaluate (True,0) env m []
          printAll (reverse pb)
          putStrLn $ show ex
          until_ en pred prompt
        (Failed s) -> do
          putStrLn s
          until_ env pred prompt
    else if (take 6 result == "import")  then do
      code <- importCode $ (\str -> return str) result
      let noCom = stripComments code
      let loc   = reverse $ map reverse $ splitProgram noCom [[]]
      let importedEnv   = runCode' loc
      until_ (mergeEnv (runCode' loc) env) pred prompt
    else do
          let monad = parse . lexer $ result
          case monad of
            (Ok m) -> do
              let (ex, en, pb) = evaluate (False,0) env m []
              printAll pb
              putStrLn (show ex)
              until_ en pred prompt
            (Failed s) -> do
              putStrLn s
              until_ env pred prompt


{-
From Data.List.Utils
| Adds the specified (key, value) pair to the given list, removing any
existing pair with the same key already present. -}
addToAL :: Eq key => [(key, elt)] -> key -> elt -> [(key, elt)]
addToAL l key value = (key, value) : delFromAL l key

{-
From Data.List.Utils
| Removes all (key, value) pairs from the given list where the key
matches the given one. -}
delFromAL :: Eq key => [(key, a)] -> key -> [(key, a)]
delFromAL l key = filter (\a -> (fst a) /= key) l

mergeEnv :: Env -> Env -> Env
mergeEnv [] orig = orig
mergeEnv (x:xs) orig = mergeEnv xs (addToAL orig (fst x) (snd x))

printAll :: [String] -> IO ()
printAll [] =  putStr ""
printAll (x:xs) = do
  putStrLn x
  printAll xs

stripComments :: String -> String
stripComments []       = []
stripComments ('#':xs) = stripComments rest
  where (line,rest)    = span (/= '\n') xs
stripComments (x:xs)   = x:stripComments xs

splitProgram :: String -> [String] -> [String]
splitProgram [] sofar     = sofar
splitProgram ('\n':xs) sofar  = splitProgram xs sofar
splitProgram s ("":xs)    = splitProgram s xs
splitProgram s sofar      = splitProgram rest (line : sofar)
                             where (line,rest) = findLine s ([],[])

findLine :: String -> (String,String) -> (String, String)
findLine ('\n':xs) (sofar,_) = (sofar,xs)
findLine ('f':'u':'n':'c':'t':'i':'o':'n':xs) _ =
    (functionBody ++ (reverse functionName) ++ "noitcnuf",rest)
    where (functionName,extra) = span (/='{') xs
          (functionBody,rest)= matchDelim extra '{' '}' 0 ("","")

findLine ('(':xs) (sofar,_)  = let (line,rest) = matchDelim ('(':xs) '(' ')' 0 ("","") in
                              (line++sofar,rest)
findLine (x:xs) (sofar,_)    = findLine xs (x:sofar,[])

--Precondition: the first character of the program must be the deliminator
matchDelim :: String -> Char -> Char -> Int -> (String,String) -> (String,String)
matchDelim (x:xs) o c 0 (before, _ ) = if before == [] then matchDelim xs o c 1 (x:"","")
                                       else (before, xs)
matchDelim [] _ _ _ _      = ("you","mismatched")
matchDelim (x:xs) open close stck (before,_)
  | x == open  = matchDelim xs open close (stck+1) (x:before,"")
  | x == close = matchDelim xs open close (stck-1) (x:before,"")
  | otherwise  = matchDelim xs open close  stck    (x:before,"")

--changes import syntax into literal path name. ("core.blah" -> core/blah.soft)
toPathName :: String -> String
toPathName s = let repl '.' = '/'
                   repl c   = c in
                   (map repl s) ++ ".soft"

--returns the first path that exists, if none exists then empty string
pathThatExists :: [String] -> String -> IO String
pathThatExists [] _         = return []
pathThatExists ("":xs) s    = pathThatExists xs s
pathThatExists (x:xs) given = do
                                let checking = x ++ given
                                b <- doesFileExist checking
                                if b then return checking
                                else pathThatExists xs given

--returns the path of a file that should be imported
getPathName :: String -> IO String
getPathName s = do
                 b <- doesFileExist s
                 if b then return s
                 else do
                        p <- readFile("paths.txt")
                        let givenPath = toPathName s
                        let defaultPaths = lines $ stripComments p
                        pathThatExists defaultPaths givenPath



--return: ([paths for import],file contents after import)
findPaths :: String -> ([String],String) -> ([String],String)
findPaths [] sofar                                  = sofar
findPaths ('\n':xs) sofar                           = findPaths xs sofar
findPaths ('i':'m':'p':'o':'r':'t':xs) (paths,_)    = findPaths rest (tail path:paths,[])
                               where   (path,rest)  = span (/= '\n') xs
findPaths rest (paths,_)                            = findPaths [] (paths,rest)

--takes list of path names, reads in each file, checks for more imports, concats files
foldCode :: [String] -> String -> IO String
foldCode [] sofar     = return sofar
foldCode (x:xs) sofar = do
                           fileName <- getPathName x
                           code <- readFile fileName
                           let (paths,rest) = findPaths code ([],[])
                           if paths == [] then foldCode xs (sofar ++ rest)
                           else foldCode (paths ++ xs)     (sofar ++ rest)
--wrapper function for foldCode
importCode :: IO String -> IO String
importCode file = do code <- file
                     let (paths,rest) = findPaths code ([],[])
                     folded <- foldCode paths ""
                     return (folded ++ rest)
main :: IO ()
main = do args <- getArgs
          case length args of
            1 -> do
              code  <- importCode $ readFile $ args !! 0
              let noCom = stripComments code
              let loc   = reverse $ map reverse $ splitProgram noCom [[]]
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
    runCodeKernel [] tokenizedInput

runCodeKernel :: Env -> [[Token]] -> IO()
runCodeKernel e [x] =
  let monad = parse x in
  case monad of
    (Ok m) ->
      let (_, _, pb) = evaluate (False,0) e m [] in
      printAll pb
    (Failed s) -> putStrLn $ show $ EErr s
runCodeKernel e (x:xs) =
  let monad = parse x in
  case monad of
    (Ok m) -> do
      let (_,env,pb) = evaluate (False,0) e m []
      printAll pb
      runCodeKernel env xs
    (Failed s) -> putStrLn $ show $  EErr s

--Same as the above definitions, but returns an enviroment. For use in REPL
runCode' :: [String] -> Env
runCode' l =
    let tokenizedInput  = map lexer l in
      runCodeKernel' [] tokenizedInput

runCodeKernel' :: Env -> [[Token]] -> Env
runCodeKernel' e [x] =
  let monad = parse x in
  case monad of
    (Ok m) ->
       snd' $ evaluate (False,0) e m []
    (Failed s) -> []
runCodeKernel' e (x:xs) =
  let monad = parse x in
  case monad of
    (Ok m) -> do
      let (_,env,_) = evaluate (False,0) e m [] in
        runCodeKernel' env xs
    (Failed s) -> []

