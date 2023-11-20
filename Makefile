all: 
	ocamlbuild -yaccflag -v -lib unix main.native #on dit de fabriquer main.native
	ln -f -s main.native fouine
	chmod +x fouine

byte: 
	ocamlbuild -yaccflag -v main.byte

clean: 
	ocamlbuild -clean
	rm -f fouine
