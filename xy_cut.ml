 	type block = {
	x : int;
	y : int;
	w : int;
	h : int
}

let create_block() = {x = 0; y = 0; w = 200; h = 100}

let get_histo img b =
	let accu_vert = Array.make b.h 0 and accu_hori = Array.make b.w 0 in
		for i = b.x to b.x + b.w - 1 do
			for j = b.y to b.y + b.h - 1 do
				if Sdlvideo.get_pixel_color img i j = (255, 255, 255) then
					accu_vert.(j) <- accu_vert.(j) + 1;
					accu_hori.(i) <- accu_hori.(i) + 1
			done
		done;
		(accu_hori, accu_vert)

let get_block (accu_hori, accu_vert) (step_x, step_y) (width, height) min =
	let blocks = ref [] in
	let n = ref 0 in
	for i = 0 to Array.length accu_vert - 1 do
		if accu_vert.(i) > min then
			incr n
		else if !n > step_y then
		begin
			blocks := [{x = 0; y = i - !n; w = width - 1; h = !n}]@(!blocks);
			n := 0
		end
	done;
	Array.of_list !blocks

let draw_blocks img blocks =
	for j = 0 to Array.length blocks - 1 do
		for i = blocks.(j).x to blocks.(j).x + blocks.(j).w do
			Sdlvideo.put_pixel_color img i blocks.(j).y (255, 0, 0);
			Sdlvideo.put_pixel_color img i (blocks.(j).y + blocks.(j).h) (255, 0, 0)
		done;
		for i = blocks.(j).y + 1 to blocks.(j).y + blocks.(j).h - 1 do
			Sdlvideo.put_pixel_color img blocks.(j).x j (0, 255, 0);
			Sdlvideo.put_pixel_color img (blocks.(j).x + blocks.(j).w) j (0, 255, 0)
		done
	done


let test_blocks img = 
 	let (width,height) = Utile.get_dims img in
 	let histo = get_histo img {x = 0; y = 0; w = width; h = height} in
 	let blocks = get_block histo (40, 40) (width, height) 625 in
		draw_blocks img blocks