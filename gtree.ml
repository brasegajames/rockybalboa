type 'a tree = {
	elt : 'a;
	mutable sons : 'a tree array;
	mutable h : int
}

let create r h = { elt = r; sons = [||]; h = h}

let root b = b.elt

let ieme_fils i b = b.sons.(i)

let height b = b.h

let get_nb_son b = Array.length b.sons

let add_elts a_elts b = let a_sons = Array.init (Array.length a_elts) (fun i -> create a_elts.(i) (b.h + 1)) in
	if Array.length b.sons = 0 then b.sons <- a_sons
	else b.sons <- Array.append b.sons a_sons

let rec update_h b =
	for i = 0 to Array.length b.sons - 1 do
		b.sons.(i).h <- b.h + 1;
		update_h b.sons.(i)
	done

let add_sons a_sons b =
	Array.iter (fun bl -> bl.h <- b.h + 1; update_h bl) a_sons;(*manque a repercuter sur les autres fils ^^*)
	if Array.length b.sons = 0 then b.sons <- a_sons
	else b.sons <- Array.append b.sons a_sons

let rec to_list b f = 
	if Array.length b.sons = 0 then [f b] else (
	let l = ref [] in
		for i = 0 to Array.length b.sons - 2 do
			l := !l@(to_list b.sons.(i) f);
			l := !l@[f b]
		done;
		l := !l@(to_list  b.sons.(Array.length b.sons - 1) f);
		!l)

let to_sons_array a_elts = Array.map (fun e -> create e 0) a_elts

(*
let b = create 5 0;;
add [|1; 3; 7|] b;;
let l = to_list b (fun b -> b.elt) in
print_string (String.concat " " (List.map string_of_int l) ^ "\n")
*)