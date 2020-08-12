MODULES=animation authors board game_screen game_state input main piece_gen piece_queue SRS tetromino utils
OBJECTS=$(MODULES:=.cmo)
MLS=$(MODULES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind

default:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

build:
	$(OCAMLBUILD) $(OBJECTS)

zip:
	zip ascii_tetris.zip *.ml* _tags Makefile *.txt config/*

test:
	$(OCAMLBUILD) -tag debug $(TEST) && ./$(TEST)

docs: docs-public 
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal \
		-html -stars -d doc.public $(MLIS)
