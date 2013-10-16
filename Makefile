EXEC= OCR.out
DIRS= -Is Pre_Treatment/ -Is -Images/ -Is Zones/
OCAMLOPT= ocamlopt
OCAMLC= ocamlc
OCAMLFLAGS= -I +sdl -I +site-lib/sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa
CAMLDEP= ocamldep
FILES= utile.mli utile.ml binarization.mli binarization.ml main.ml Pre_Treatment/rotation.ml Zones/zones.ml Zones/zones.mli

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
