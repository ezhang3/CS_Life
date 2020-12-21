MODULES= playerstate board authors tile specialevents gui
FILES= main
OBJECTS=$(MODULES:=.cmo) $(FILES:=.cmo)
MLS=$(MODULES:=.ml) $(FILES:=.ml)
MLIS=$(MODULES:=.mli)
TEST=test.byte
MAIN=main.byte
GUI=gui.byte
OCAMLBUILD=ocamlbuild -use-ocamlfind -pkg graphics

default: build
	utop

build:
	$(OCAMLBUILD) $(OBJECTS)

test:
	$(OCAMLBUILD) -tag 'debug' $(TEST) && ./$(TEST)

play:
	$(OCAMLBUILD) $(MAIN) && ./$(MAIN)

gui:
	$(OCAMLBUILD) $(GUI) && ./$(GUI)

check:
	bash checkenv.sh && bash checktypes.sh
	
finalcheck: check
	bash checkzip.sh
	bash finalcheck.sh

zip:
	zip life.zip *.md* *.ml* *.json _tags Makefile
	
docs: docs-public docs-private
	
docs-public: build
	mkdir -p doc.public
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal,graphics \
		-html -stars -d doc.public $(MLIS)

docs-private: build
	mkdir -p doc.private
	ocamlfind ocamldoc -I _build -package yojson,ANSITerminal,graphics \
		-html -stars -d doc.private \
		-inv-merge-ml-mli -m A $(MLIS) $(MLS)

clean:
	ocamlbuild -clean
	rm -rf doc.public doc.private life.zip