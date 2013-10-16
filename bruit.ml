(*
let is_in_bounds x y img =
	let (w,h) = Utile.get_dims img in
		x >= 0 && x < w && y >= 0 && y < h

let coupe_mediane img dst =
	let (h, w) = Utile.get_dims img in
	for i = 0 to h-1 do
	for j = 0 to w-1 do
		let tab =  ref (Array.make 9 (999,999,999)) in
		let nb = ref 0 in
		for x = -1 to 1 do
		for y = -1 to 1 do
			if (is_in_bounds (i+x) (j+y) img) then
					let (r,g,b) = Sdlvideo.get_pixel_color img (i+x) (j+y) in
					!tab.(nb) := (r,g,b);
					nb := !nb + 1;
		done
		done;
		while (!tab.(nb) = (999,999,999)) do
			nb = !nb-1;
			tab = Array.sub !tab 0 !nb;
		done;
		let sorted_tab = Array.sort !tab in
		Sdlvideo.put_pixel_color dst i j (sorted_tab.(List.length sorted_liste /2));
	done
	done
*)

let dropthebass n src =
let (w,h) = Utile.get_dims src in
  for i = 0 to h-1 do
    for j = 0 to w-1 do
     	let (rm, rg, rb) = ref (0,0,0) in
     	let 
     	for x = -1 to 1 do
        	for y = -1 to 1 do
        		let a = ref( i + x) in
        		let c = ref ( j + y) in
          			if (!a < 0) then a := 0;
          			if (!a > w - 1) then a := w-1;
          			if (!c < 0) then c := 0;
          			if (!c > h - 1) then c := h-1;    
          			let (r, g, b) = Sdlvideo.get_pixel_color src !a !c in
          				moy := !moy + (r);
         	done;
      	done;
      moy := !moy / (n*n);
      Sdlvideo.put_pixel_color copy (!moy,!moy,!moy);
     done;
   done;
