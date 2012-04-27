all: masterzone.beam parser.beam parser_grammar.beam

%.beam: %.erl
	erlc $<

parser_grammar.erl: parser_grammar.peg
	erl -noshell -eval "neotoma:file(\"parser_grammar.peg\")" -s erlang halt

clean:
	-rm *.beam
	-rm parser_grammar.erl