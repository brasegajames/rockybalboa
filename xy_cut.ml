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
	while !i < Array.length a && !nb_blanc < step do
		if a.(!i) >= seuil then
			incr nb_blanc
		else
			nb_blanc := 0;
		incr i
	done;
	let block =		
		if vert then
		 	{ x = b.x; y = b.y + init; w = b.w; h = !i - init - !nb_blanc - 1 }
		else
			{ x = b.x + init; y = b.y; w = b.x + !i - init - !nb_blanc - 1; h = b.h } in
			blocks := block::(!blocks);
	!i - !nb_blanc

let get_block a block step vert =
	(* Printf.printf "get_block\n"; *)
	let blocks = ref [] in
	let seuil = (if vert then block.w - 3 else block.h - 10)(*a.(Histo.histogram_median a) * 120 / 100*) in
	(* Printf.printf "seuil: %d\n" seuil; *)
	let i = ref 0 in
	while !i < Array.length a do
		(*Printf.printf "a.(%d) : %d\n" !i a.(!i);*)
		if a.(!i) < seuil then
		 	i := add_block a block !i step seuil vert blocks;
		incr i
	done;
	(* Printf.printf "nbblocks: %d\n" (List.length !blocks); *)
	Array.of_list !blocks

let draw_blocks img blocks =
	for j = 0 to Array.length blocks - 1 do
		for i = blocks.(j).x to blocks.(j).x + blocks.(j).w do
			Sdlvideo.put_pixel_color img i blocks.(j).y (255, 0, 0);
			Sdlvideo.put_pixel_color img i (blocks.(j).y + blocks.(j).h) (255, 0, 0)
		done;
		for i = blocks.(j).y to blocks.(j).y + blocks.(j).h do
			Sdlvideo.put_pixel_color img blocks.(j).x i (0, 255, 0);
			Sdlvideo.put_pixel_color img (blocks.(j).x + blocks.(j).w) i (0, 255, 0)
		done
	done

let xy_cut =
	let vert = ref false in
		fun img b ->
		let (accu_hori, accu_vert) = get_histo img b in
 		let blocks = get_block (if !vert then accu_vert else accu_hori) b 10 !vert in
 			vert := not !vert;
 			(*draw_blocks img blocks;*)
 			for i = 0 to Array.length blocks - 1 do
 				let (accu_hori, accu_vert) = get_histo img blocks.(i) in
 					let blocks2 = get_block (if !vert then accu_vert else accu_hori) blocks.(i) 20 !vert in
 						draw_blocks img blocks2;
 				(*xy_cut img blocks.(i)*)
 			done

let rec xy_cut_rec = 
		fun img b steps n v ->
		let (accu_hori, accu_vert) = get_histo img b in
 		let blocks = get_block (if v then accu_vert else accu_hori) b steps.(0) v in

			if n = 0 then
 				draw_blocks img blocks
 			else
 				for i = 0 to Array.length blocks - 1 do
	 				xy_cut_rec img blocks.(i) (Array.sub steps 1 (Array.length steps - 1)) (n - 1) (not v);
	 			done

let test_blocks img = 
 	let (width,height) = Utile.get_dims img in 
 		xy_cut_rec img { x = 0; y = 0; w = width; h = height } [|10; 25; 2; 20|] 2 true
 																(* pas      nb_iter  true:vert*)