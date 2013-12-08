EXEC= OCR
DIRS= -Is Pre_Treatment/ -Is -Images/ -Is Zones/
OCAMLOPT= ocamlopt
OCAMLC= ocamlc
OCAMLFLAGS= -I +lablgtk2 -I +site-lib/lablgtk2 -I +sdl -I +site-lib/sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa lablgtk.cmxa gtkInit.cmx lablgtkspell.cmxa
CAMLDEP= ocamldep
FILES= utile.mli utile.ml gtree.ml matrix.ml bruit.mli bruit.ml binarization.mli binarization.ml rotation.mli rotation.ml xy_cut.ml featuring.ml main.ml menu_entry.ml

OCR: ${FILES}
	${OCAMLOPT} -w s ${OCAMLFLAGS} ${OCAMLLD} -o ${EXEC} ${FILES}

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
