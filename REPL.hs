import Prelude 
import SOFTGrammar
import SOFTLexer
main = getContents >>= print . parse . lexer




