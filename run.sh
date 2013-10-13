#!/bin/bash

ocaml -I +sdl -I +site-lib/sdl bigarray.cma sdl.cma sdlloader.cma -init $1
