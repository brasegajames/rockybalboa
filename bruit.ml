let is_in_bounds x y img =
	let (w,h) = Utile.get_dims img in
  if (x >= 0 && x < w && y >= 0 && y < h) then
    true
  else
    false

let coupe_mediane img dst =
	let (h, w) = Utile.get_dims img in
	for i = 0 to h-1 do
	for j = 0 to w-1 do
		let li =  ref [] in
		for x = -3 to 1 do
		for y = -1 to 1 do
			if (is_in_bounds (i+x) (j+y) img) then
					li := (Sdlvideo.get_pixel img (i+x) (j+y))::!li;
		done;
		done;
		let sorted_li = Utile.merge_sort !li in
		Sdlvideo.put_pixel dst i j (List.nth sorted_li ((List.length sorted_li) /2));
	done
	done

let annex (r,g,b) factor = (factor * r, factor * g, factor * b) 

let annex2 (a,b,c) (d,e,f) = (a+d, b+e, c+f)

let annex3 (r,g,b) div = (r/div, g/div, b/div)

let final (r,g,b) = match (r,g,b) with
  |(x,_,_) when x < 0 -> (0,g,b);
  |(x,_,_) when x > 255 -> (255,g,b);
  |(_,y,_) when y < 0 -> (r,0,b);
  |(_,y,_) when y > 255 -> (r,255,b);
  |(_,_,z) when z < 0 -> (r,g,0);
  |(_,_,z) when z > 255 -> (r,g,255);
  |(x,y,z) -> (x,y,z)

let apply_mat matrice x y src =
  let acu = ref (0,0,0) in
  let div = ref 0 in
  for i = 0 to 2 do
    for j = 0 to 2 do
      let factor = matrice.(i).(j) in
        acu := annex2 !acu  (annex (Sdlvideo.get_pixel_color src x y) factor);
        div := !div +1;
    done;
  done;
  while ((final (annex3 !acu !div)) = !acu) do
    acu := final (annex3 !acu !div);
  done;
  !acu
  
let matrix_int = Array.make_matrix 3 3 0

let flou_gaussien img dst =
  let (h, w) = Utile.get_dims img in
  matrix_int.(0).(0) <- -1;
  matrix_int.(0).(1) <- -1;
  matrix_int.(0).(2) <- -1;
  matrix_int.(1).(0) <- -1;
  matrix_int.(1).(1) <- 9;
  matrix_int.(1).(2) <- -1;
  matrix_int.(2).(0) <- -1;
  matrix_int.(2).(1) <- -1;
  matrix_int.(2).(2) <- -1;
  for i = 0 to h-1 do
  for j = 0 to w-1 do
    Sdlvideo.put_pixel_color dst i j (apply_mat matrix_int i j img);
  done
done



(*
let dropthebass n src =
let (w,h) = Utile.get_dims src in
  for i = 0 to h-1 do
    for j = 0 to w-1 do
     	let (rm, rg, rb) = (0,0,0) in
     	let nb = 0 in
     	for x = -1 to 1 do
        	for y = -1 to 1 do
        		let a = ( i + x) in
        		let c = ( j + y) in
        			if (is_in_bounds a c src) then
        				nb <- nb +1;
          			let (r, g, b) = Sdlvideo.get_pixel_color src a c in
          				rm <- rm + r;
          				rg <- rg + g;
          				rb <- rb + b;
         	done;
      	done;
    	  rm = rm / nb;
   	    rg = rg / nb;
        rb = rb / nb;
      Sdlvideo.put_pixel_color copy (rm, rg, rb);
     done;
   done;
   *)