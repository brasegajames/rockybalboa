let is_in_bounds x y img =
	let (w,h) = Utile.get_dims img in
		x >= 0 && x < w && y >= 0 && y < h
(*
let coupe_mediane img dst =
	let (h, w) = Utile.get_dims img in
		for i = 0 to h-1 do
			for j = 0 to w-1 do
				let liste = [] in
				let sorted_liste = [] in
				for a = -1 to 1 do
					for b = -1 to 1 do
						if (is_in_bounds (i+a) (j+b) img) then
							(Sdlvideo.get_pixel_color img i j)::liste
					done;
				done;
				Utile.merge_sort liste sorted_liste;
				Sdlvideo.put_pixel_color dst i j (List.nth (sorted_liste.length/2));
			done
		done *)