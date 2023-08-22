# AnnB translation tool

This tool is used to analyse and translate the AnnB language into an input language for the noname tool. In order to run the code, it has to be first compiled: `ghc -o annb Main.hs`, which creates an executable `annb` that can then be run on an AnnB file. The program then prints the noname protocol definition into the stdout.

### Parser and Lexer updating

In the case that there needs to be a change to the lexer file (`AnnB_Lexer.x`) or parser file (`AnnB_Parser.y`), the relevant tool has to be ran to regenerate the source code. For the lexer file this is: `alex AnnB_Lexer.x` and for parser: `happy AnnB_Parser.y`.