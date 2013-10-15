OCAML=ocamlopt
OCAMLFLAGS= -I +sdl -I +site-lib/sdl
OCAMLLD= bigarray.cmxa sdl.cmxa sdlloader.cmxa
FILES= utile.ml bruit.ml main.ml 

OCR: ${FILES}
	${OCAML} ${OCAMLFLAGS} ${OCAMLLD} -o OCR ${FILES}
 
clean::
	rm -f *~ *.o *.cm? OCR
