derivedFiles=SOFTGrammar.hs
all: clean $(binaries)

buildGrammar: 
	happy SOFTGrammar.y 

.PHONY: clean

clean:
	rm $(derivedFiles)
