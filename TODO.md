https://www.cmi.ac.in/~spsuresh/teaching/prgh15/papers/monadic-parsing.pdf

- change Maybe to Either, and provide user with error messages
- implement expression parsing just like in jq (https://stedolan.github.io/jq/)
- configure program to work with input both from files AND stdin
- configure program to be able to write output both to files AND stdout
- check that pipelines work and fix them otherwise
- structurize code so that other programmers will be able to import that to their projects
- implement feature to split one json to separate files (and vice versa). Figure out how to decide splitting method
- add option to just validate given json (just parse it) (maybe minimify or prettify by default)
- (MAYBE) export json to .dot file, with ability to merge objects with parameters that apply to given pattern, or set different colors for different nodes (also set by some parameter).
```Example:
Each node that has 'value1' set to true/any (somewhere inside or on top level - will be determined by given pattern) - blue
Otherwise - yellow
```
- (MAYBE) implement translation from json to xml (and vice versa?)
- (MAYBE) visualise json as a tree (what for?)
- (MAYBE) search of big duplicates of objects (size (or depth level) is variable and passed through cli argument)

