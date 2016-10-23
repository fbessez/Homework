These files contain the lexer and parser for a fragment of C written in the Standard ML of New Jersey Meta Language.

In order to run this, 

First, run the makefile with target 'driver' --> 'make driver'

Then, run './driver --lex FILENAME'
	, run './driver --expr FILENAME'
	, run './driver --arg "int x = 5;'
	depending on whether you want to test the lexer, parser of an expression, or parser of a decleration/program

