SRC=src/Parser.hs src/Generator.hs src/JsonParser.hs src/QueryParser.hs src/Main.hs src/CommandLine.hs
EXEC=bin/Main # TODO: figure out better name
FLAGS=-O

# run: compile
#	./$(EXEC)

compile: $(SRC)
	ghc $(SRC) $(FLAGS) -o $(EXEC)

clean:
	rm -rf *~ src/*.o src/*.hi src/*~ \
	$(EXEC)

.PHONY: clean compile run
