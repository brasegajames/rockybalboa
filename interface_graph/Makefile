EXEC= inter.out
OCAMLC= ocamlc
OCAMLFLAGS= -I +lablgtk2 -I +sdl -I +site-lib/sdl
OCAMLLD= lablgtk.cma gtkInit.cmo lablgtkspell.cma
CAMLDEP= ocamldep
FILES= menu_entry.ml

INTER: ${FILES} ### -w s = ne pas afficher les warning unit 
	${OCAMLC} -w s ${OCAMLFLAGS} -o ${EXEC} ${OCAMLLD} ${FILES}

.SUFFIXES :
	.ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLFLAGS) -c $<

clean::
	rm -f *~ *.o *.cm? ${EXEC}
