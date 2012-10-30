#################### 
GROUP_NUMBER := 14
####################

ERLC := erlc
ERLC_FLAGS := -W -I include

ERL_FILES := $(wildcard src/*.erl) src/parser_grammar.erl
BEAM_FILES := $(patsubst src/%.erl,ebin/%.beam,${ERL_FILES})

comma:= ,
empty:=
space:= $(empty) $(empty)

EDOC_SRC := $(filter-out %_test.erl, $(ERL_FILES))
EDOC_SRC_LIST := [$(subst $(space),$(comma),$(patsubst src/%.erl,'src/%.erl', $(EDOC_SRC)))]

REQUIRED_DIR_NAME := pop_2012_project_group_$(GROUP_NUMBER)

PROJECT_DIR := $(notdir $(shell pwd))

USER=$(shell whoami)
RANDOMCLIENT= kidclient$(shell bash -c "echo \$$RANDOM")
MACHINE=$(shell hostname)
SERVER='kidserver@$(MACHINE)'
ARCHIVE_NAME :=  $(REQUIRED_DIR_NAME)_archive_$(USER)_$(shell date "+%Y-%m-%d__%H:%M:%S")__.tar.gz
ARCHIVE_DIR := ..

all: $(BEAM_FILES)

ebin/%.beam: src/%.erl
	@-mkdir -p ebin
	$(ERLC) $(ERLC_FLAGS) -o ebin $<

start_client: all
	@bash -c "echo $$RANDOM"
	@echo "Connect by typing \"client:connect($(SERVER)).\""
	@echo " and pressing enter."
	@echo " ------ "
	erl -name $(RANDOMCLIENT) -pa ebin -setcookie kid-mud

start_server: all
	erl -name kidserver -setcookie kid-mud -pa ebin -mnesia dir database -eval "application:start(mnesia)" -eval "application:start(kidmud)"

start_webserver: all
	mkdir -p yaws_logs
	@echo "The webserver will listen to port 8080"
	yaws -i --conf yaws.conf --name kidserver --mnesiadir database

setup: all
	mkdir database
	erl -name kidserver -noshell -pa ebin -mnesia dir database -s database setup -s erlang halt

src/parser_grammar.erl: src/parser_grammar.peg
	erl -noshell -pa neotoma/ebin -eval "neotoma:file(\"src/parser_grammar.peg\")" -s erlang halt

test: all
	erl -noinput -pa ebin -eval 'eunit:test({dir, "ebin/"}, [verbose]), init:stop()'

doc: $(BEAM_FILES)
	erl -noshell -eval "edoc:files($(EDOC_SRC_LIST), [{dir, 'doc/html'}])" -s init stop

clean:
	rm -rf .#* *.dump
	rm -rf ebin/*.beam
	rm -f src/parser_grammar.erl
	-(cd doc/html && find . -name "*" -a ! -name overview.edoc -exec rm -rf {} \;)
