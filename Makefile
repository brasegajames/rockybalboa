EXEC= OCR.out
OCAMLOPT= ocamlopt
OCAMLC= ocamlc
OCAMLFLAGS= -I +sdl -I +site-lib/sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa
CAMLDEP= ocamldep
FILES= utile.mli utile.ml binarization.mli binarization.ml bruit.mli bruit.ml main.ml

OCR: ${FILES}
	${OCAMLOPT} ${OCAMLFLAGS} ${OCAMLLD} -o ${EXEC} ${FILES}

.SUFFIXES :
	.ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<*.ml

clean::
	rm -f *~ *.o *.cm? ${EXEC}

depend:
	${CAMLDEP} ${OCAMLFLAGS} *.mli *.ml > .depend

include .depend
