derivedFiles=SOFTGrammar.hs soft

buildGrammar: 
	happy SOFTGrammar.y 
	ghc SOFTRun.hs -o soft
.PHONY: clean

all: clean $(binaries)
clean:
	rm $(derivedFiles)
	rm $(binaries) *.o *.hi
