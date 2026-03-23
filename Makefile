# Makefile du projet de INPF12
# Pour compiler le projet taper simplement make en étant dans le répertoire du projet
# Pour nettoyer les fichiers générés taper make clean
# Ne pas modifier ce fichier à moins de comprendre ce que vous faites (ce qui n'est sans doute pas le cas)

#OCAMLLIBS=-I /pub/FISE_PRFO23/ocaml/graphics  -dllpath /pub/FISE_PRFO23/ocaml/stublibs/ -I /pub/FISE_PRFO23/ocaml/stublibs/
OCAMLLIBS=-I +graphics

FICHIER=projet.ml 
BIN=editeur

test: projet.cmo
	ocamlc -g $(OCAMLLIBS) graphics.cma projet.cmo -o $@


$(BIN): projet.cmo
	ocamlc -g $(OCAMLLIBS) graphics.cma projet.cmo -o $@

%.cmo : %.ml 
	ocamlc -g $(OCAMLLIBS) -c $<

%.cmi : %.mli
	ocamlc -g $(OCAMLLIBS) -c $<


clean : 
	rm -f projet.cmo projet.cmi $(BIN) *~
