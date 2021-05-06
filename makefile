SRC=src/*.hs
EXEC=bin/hson # TODO: figure out better name
FLAGS=-O2

# run: compile
#	./$(EXEC)

compile: $(SRC)
	ghc $(SRC) $(FLAGS) -o $(EXEC)

clean:
	rm -rf *~ src/*.o src/*.hi src/*~ \
	$(EXEC)

.PHONY: clean compile run
