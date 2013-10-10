(* Des trucs qui peuvent être utiles *)

let make_matrix n1 n2 init =
	Array.init n1 (fun _ -> Array.Make n2 init)

let get_dim img =
	((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)

let level (r,g,b) =
	int_of_float((0.299 *. r) +. (0.587 *. g) +. (0.114 *. b))
