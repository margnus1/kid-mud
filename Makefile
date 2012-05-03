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
ARCHIVE_NAME :=  $(REQUIRED_DIR_NAME)_archive_$(USER)_$(shell date "+%Y-%m-%d__%H:%M:%S")__.tar.gz
ARCHIVE_DIR := ..

all: $(BEAM_FILES)

ebin/%.beam: src/%.erl
	@-mkdir -p ebin
	$(ERLC) $(ERLC_FLAGS) -o ebin $<

start_client: all
	@echo "Connect by typing \"client:connect('kidserver@<Server-Name>').\""
	@echo " and pressing enter (replace <Server-Name> with the name of the server)."
	@echo " ------ "
	erl -sname kidclient -pa ebin

start_server: all
	erl -sname kidserver -noshell -pa ebin -mnesia dir database -s database -s zonemaster

setup: all
	mkdir database
	erl -sname kidserver -noshell -pa ebin -mnesia dir database -s database setup -s erlang halt

src/parser_grammar.erl: src/parser_grammar.peg
	erl -noshell -pa neotoma/ebin -eval "neotoma:file(\"src/parser_grammar.peg\")" -s erlang halt

test: all
	(cd ebin && erl -noinput -eval 'eunit:test({dir, "."}, [verbose]), init:stop()')

doc: $(BEAM_FILES)
	erl -noshell -eval "edoc:files($(EDOC_SRC_LIST), [{dir, 'doc/html'}])" -s init stop

clean:
	rm -rf .#* *.dump
	rm -rf ebin/*.beam
	rm -f src/parser_grammar.erl
	(cd doc/html && find . -name "*" -a ! -name overview.edoc -exec rm -rf {} \;)

remove_finderinfo:
	-xattr -d "com.apple.FinderInfo" src/*.erl include/*.hrl doc/* doc/html/*

archive: clean
ifeq ($(REQUIRED_DIR_NAME), $(PROJECT_DIR))
	(cd $(ARCHIVE_DIR) && tar cvfz $(ARCHIVE_NAME) $(PROJECT_DIR) )
	@echo 
	@echo NOTE: Archive created in $(ARCHIVE_DIR)/$(ARCHIVE_NAME)
	@echo 
else
	@echo Error: Wrong directory name >$(PROJECT_DIR)<, change to >$(REQUIRED_DIR_NAME)<
endif

