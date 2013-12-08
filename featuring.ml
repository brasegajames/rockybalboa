type feat = Empty | Char of char | Skelet of int array array

(*m etant la lettre sortie du xy cut, revoie son skelette dans une matrice*)
(* la matrice est binaire: 0 noir, 1 blanc*)
let get_feat m = m

let eval_mat_char mat_char = 
	match mat_char with
	| Xy_cut.Empty -> Empty
	| Xy_cut.Char(c) -> Char(c)
	| Xy_cut.MatChar(m) -> Skelet(get_feat m)
	
let matchar_array_to_featarray mat_char_array = Array.map (fun x -> eval_mat_char x) mat_char_array