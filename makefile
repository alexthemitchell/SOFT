derivedFiles=SOFTGrammar.hs soft

make: 
	happy SOFTGrammar.y 
	ghc SOFTRun.hs -o soft

.PHONY: clean

all: clean $(binaries)
clean:
	rm -f $(derivedFiles)
	rm -f *.o *.hi

.PHONY: test

test:
	./soft tests/grammar.soft
