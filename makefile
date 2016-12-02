derivedFiles=SOFTGrammar.hs

buildGrammar: 
	happy SOFTGrammar.y 

.PHONY: clean

all: clean $(binaries)
clean:
	rm $(derivedFiles)
