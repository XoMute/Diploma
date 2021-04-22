https://www.cmi.ac.in/~spsuresh/teaching/prgh15/papers/monadic-parsing.pdf

- Implement correct FUCKING errors
```Try next principle:
If any monadic parser parsed his first token, and then failed - consider it as a complete fail.
But be sure to check if this will work for all parsers and tokens: maybe some parsers start the same, but end differently.
```
- implement expression parsing just like in jq (https://stedolan.github.io/jq/)
-- implement semantic check for successfully parsed queries
-- NOTE: Expression parsing should be very limited (not so enormous like in jq), and should be inputted after some parameter (like '-e') was passed to main program. All following features should be done with separate parameters (it will be much more easier?)
- expression or cli option to delete some values from objects (instead of reconstructing them without this value via '-e') (?)
- implement feature to split one json to separate files (and vice versa). Figure out how to decide splitting method (try to implement that with '-e')
- add rules that user can input to validate some jsons (like, some key has to be present, or value of some key should be of some exact type, or it has to have some given value, or array should be with length of N, or array is homogenous etc.
- (MAYBE) export json to .dot file, with ability to merge objects with parameters that apply to given pattern, or set different colors for different nodes (also set by some parameter).
```
Example:
Each node that has 'value1' set to true/any (somewhere inside or on top level - will be determined by given pattern) - blue
Otherwise - yellow
```
- (MAYBE) implement translation from json to xml (and vice versa?)
- (MAYBE) search of big duplicates of objects (size (or depth level) is variable and passed through cli argument)

- Add option to parse only toplevel
- Recursively search for object with given field and value
  * Add option to show parent of found object (like -P1 or -P5)
