SRC=src/Parser.hs src/Generator.hs src/JsonParser.hs src/QueryParser.hs src/Main.hs src/CommandLine.hs
EXEC=bin/Main

# run: compile
#	./$(EXEC)

compile: $(SRC)
	ghc $(SRC) -o $(EXEC) # TODO: figure out better name

clean:
	rm -rf *~ src/*.o src/*.hi src/*~ \
	$(EXEC)

.PHONY: clean compile run
