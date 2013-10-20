type block = {
	x : int;
	y : int;
	w : int;
	h : int
}

(*b = block*)
let get_histo img b =
	let accu_vert = Array.make h 0 and accu_hori = Array.make w 0 in
		for i = b.x to b.x + b.w - 1 do
			for j = b.y to b.y + b.h - 1 do
				if Sdlvideo.get_pixel_color img i j = (255, 255, 255) then
					accu_vert.(j) <- accu_vert.(j) + 1;
					accu_hori.(i) <- accu_hori.(i) + 1
			done
		done;
		(accu_hori, accu_vert)

let get_block (accu_hori, accu_vert) (step_x, step_y) (w, h) min =
	let blocks = [] in
	let n = ref 0 in
	for i = 0 to Array.Length accu_vert - 1 do
		if accu_vert.(i) > min then
			incr n
		else if (n > step_y) then
			({x = 0; y = i - n; width = w; height = n})::blocks;
			n := 0
	done;
	blocks

let draw_blocks img blocks =
	let b = ref List.hd blocks in
		while !b != [] do
			for i = b.x to b.x + b.w do
				Sdl.put_pixel_color img i b.y (255, 0, 0);
				Sdl.put_pixel_color img i (b.y + b.h) (255, 0, 0)
			done;
			for j = b.y + 1 to b.y + b.h - 1 do
				Sdl.put_pixel_color img b.x j (0, 255, 0);
				Sdl.put_pixel_color img (b.x + b.w) j (0, 255, 0)
			done;

			b := List.hd blocks
		done

let test_blocks img = 
 	let (width,height) = Utile.get_dims img in
		draw_blocks img (get_block 
			(get_histo img {x = 0; y = 0; w = width; h = height}))	