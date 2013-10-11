(* Des trucs qui peuvent être utiles *)

(*
let make_matrix n1 n2 init =
	Array.init n1 (fun _ -> Array.Make n2 init)

=> Inutile 
val make_matrix : int -> int -> 'a -> 'a array array
*)

let get_dim img =
	((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)

let level (r,g,b) =
	int_of_float((0.299 *. r) +. (0.587 *. g) +. (0.114 *. b))

(* fonctions pour le tri fusion (en: merge_sort) *)

let rec split = function
	| [] -> ([],[])
	| [e] -> ([e],[])
	| (premier::second::reste) -> let (a,b) = split reste in 
                              (premier::a,second::b)

let rec merge liste1 liste2 =
    match liste1,liste2 with
    |[],_->liste2
    |_,[]->liste1
    |t1::q1,t2::q2->
        if t1<t2 then
            t1::(merge q1 liste2)
        else
            t2::(merge liste1 q2)

let rec merge_sort = function
     | [] -> []
     | [e] -> [e]
     | liste -> let (e1,e2) = split liste in
                            merge (merge_sort e1) (merge_sort e2)