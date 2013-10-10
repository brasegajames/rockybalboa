(* Des trucs qui peuvent être utiles *)

let make_matrix n1 n2 init =
	Array.init n1 (fun _ -> Array.Make n2 init)