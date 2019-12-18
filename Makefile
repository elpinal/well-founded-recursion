all: lexer.sml grammar.sml grammar.sig
	mosmlc -liberal -toplevel std.sml ord.sml set.sml pretty.sml syntax.sml env.sml semantics.sml surface.sml -structure grammar.sig grammar.sml lexer.sml main.sml

run: all
	./a.out

lexer.sml: lexer.lex
	mosmllex lexer.lex

grammar.sml grammar.sig: grammar.grm
	mosmlyac grammar.grm

clean:
	rm *.ui *.uo a.out

.PHONY: all run clean
