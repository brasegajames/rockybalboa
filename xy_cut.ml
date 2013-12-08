 	type block = {
	x : int;
	y : int;
	w : int;
	h : int
}

let create_block() = {x = 0; y = 0; w = 200; h = 100}

let get_histo img b =
	let accu_vert = Array.make b.h 0 and accu_hori = Array.make b.w 0 in
		for i = 0 to b.w - 1 do
			for j = 0 to b.h - 1 do
			let (r, g, b) = Sdlvideo.get_pixel_color img (i + b.x) (j + b.y) in
				if (r, g, b) = (255, 255, 255) then (
				accu_vert.(j) <- accu_vert.(j) + 1;
				accu_hori.(i) <- accu_hori.(i) + 1)
			done
		done;
		(accu_hori, accu_vert)

let add_block a b init step seuil vert blocks =
	let nb_blanc = ref 0 in
	let i = ref init in
	(*Printf.printf "add_blocks test: %d %d %d %d %d\n" b.x b.y b.w b.h init;*)
	while !i < Array.length a && !nb_blanc < step do
		if a.(!i) >= seuil then
			incr nb_blanc
		else
			nb_blanc := 0;
		incr i
	done;
		let block =
				if vert then
				 	{ x = b.x; y = b.y + init - 1; w = b.w; h = !i - init - !nb_blanc + 1 }
				else
					{ x = b.x + init - 1; y = b.y; w = !i - init - !nb_blanc + 1; h = b.h } in
					if block.w > 2 && block.h > 2 then 
						blocks := block::(!blocks);
	!i - 1

let get_block a block step vert =
	let blocks = ref [] in
	let seuil = (if vert then block.w - 1 else block.h - 1)(*a.(Histo.histogram_median a) * 120 / 100*) in
	(*Printf.printf "seuil: %d\n" seuil;*)
	let i = ref 0 in
	while !i < Array.length a do
		(*Printf.printf "a.(%d) : %d\n" !i a.(!i);*)
		if a.(!i) < seuil then
		 	i := add_block a block !i step seuil vert blocks;
		incr i
	done;
	(*Printf.printf "nbblocks: %d\n" (List.length !blocks);*)
	Array.of_list !blocks

let draw_blocks img blocks =
	for j = 0 to Array.length blocks - 1 do
		(*Printf.printf "block: %d %d %d %d\n" blocks.(j).x blocks.(j).y blocks.(j).w blocks.(j).h;*)
		for i = blocks.(j).x to blocks.(j).x + blocks.(j).w do
			Sdlvideo.put_pixel_color img i blocks.(j).y (255, 0, 0);
			Sdlvideo.put_pixel_color img i (blocks.(j).y + blocks.(j).h) (255, 0, 0)
		done;
		for i = blocks.(j).y to blocks.(j).y + blocks.(j).h do
			Sdlvideo.put_pixel_color img blocks.(j).x i (0, 255, 0);
			Sdlvideo.put_pixel_color img (blocks.(j).x + blocks.(j).w) i (0, 255, 0)
		done
	done

let rec xy_cut_rec = 
		fun img b steps n v ->
		let (accu_hori, accu_vert) = get_histo img b in
 		let blocks = get_block (if v then accu_vert else accu_hori) b steps.(0) v in

			if n = 0 then (
 				draw_blocks img blocks;
 				Gtree.to_sons_array blocks
 				)
 			else (
 				let a_sons = Gtree.to_sons_array blocks in
 				for i = 0 to Array.length blocks - 1 do
 					let sons = xy_cut_rec img blocks.(i) (Array.sub steps 1 (Array.length steps - 1)) (n - 1) (not v) in
	 				Gtree.add_sons sons a_sons.(i);
	 			done;
	 			a_sons
	 		)

let block_to_mat b img = Matrix.of_img (fun (r, g, b) -> r/255) img (b.x, b.y, b.w, b.h)

type mat_char = Empty | Char of char | MatChar of int array array

let eval_node img tree = 
	if Gtree.get_nb_son tree = 0 then (* si feuille*)
		MatChar(block_to_mat (Gtree.root tree) img)
	else if Gtree.height tree = 2 then
		Char('\n')
	else if Gtree.height tree = 3 then
		Char(' ')
	else
		Empty

let blocktree_to_list_of_mat tree img = Gtree.to_list tree (eval_node img)

let get_tree_blocks img n = 
 	let (width,height) = Utile.get_dims img in 
 		let tree = Gtree.create { x = 0; y = 0; w = width; h = height } 0 in 
 		let blocks = xy_cut_rec img { x = 0; y = 0; w = width; h = height } [|10; 15; 2; 4; 30; 1; 10|] n true in
 		Gtree.add_sons blocks tree;
 		Array.of_list (blocktree_to_list_of_mat tree img)
 		(*xy_cut_rec img { x = 18; y = 719; w = 300; h = 15 } [|1; 4; 2; 1|] 0 false; *)
 																(* pas      nb_iter  true:vert*)