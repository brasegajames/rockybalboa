let is_in_bounds x y img =
	let (w,h) = Utile.get_dims img in
  if (x >= 0 && x < w && y >= 0 && y < h) then
    true
  else
    false

(* Associer median puis gaussien pour enlever le bruit, c'est not bad *)

let median img dst =
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

let annex3 (r,g,b) div plus = (r/div + plus, g/div + plus , b/div + plus)

let final (r,g,b) = match (r,g,b) with
  |(x,_,_) when x < 0 -> (0,g,b);
  |(x,_,_) when x > 255 -> (255,g,b);
  |(_,y,_) when y < 0 -> (r,0,b);
  |(_,y,_) when y > 255 -> (r,255,b);
  |(_,_,z) when z < 0 -> (r,g,0);
  |(_,_,z) when z > 255 -> (r,g,255);
  |(x,y,z) -> (x,y,z)

let choice = function
  |"medium" -> [| [|1;1;1 |] ;[|1; 0;1 |] ; [|1;1; 1|] |]
  |"eroder" -> [| [|0; 1; 0 |] ;[|1; 1; 1 |] ; [|0; 1; 0 |] |]
  |"contrast" -> [| [|0;-1;0 |] ;[|-1; 5;-1 |] ; [|0;-1; 0|] |]
  |"flou" -> [| [|1; 1; 1 |] ;[|1;1;1 |] ; [|1; 1; 1 |] |]
  |"bord" -> [| [|0; 0; 0 |] ;[|-1; 1; 0 |] ; [|0; 0; 0 |] |]
  |"bordplus" -> [| [|0; 1; 0 |] ;[|1; -4; 1 |] ; [|0; 1; 0 |] |]
  |"repoussage" -> [| [|-2; -1; 0 |] ;[|-1; 1; 1 |] ; [|0; 1; 2 |] |]
  |"gaussien" -> [| [|1; 2; 1 |] ;[|2; 4; 2|] ; [|1; 2; 1 |] |]
  |_ -> [| [|0; 0; 0 |] ;[|0; 1; 0 |] ; [|0; 0; 0 |] |]

let apply_mat matrice x y src =
  let acu = ref (0,0,0) in
  let div = ref 0 in
  let plus = ref 0 in
  let mat = choice matrice in
  for i = 0 to 2 do
    for j = 0 to 2 do
      let factor = mat.(i).(j) in
      if (is_in_bounds (x+i-1) (y+j-1) src) then
      begin        
        acu := annex2 !acu  (annex (Sdlvideo.get_pixel_color src (x+i-1) (y+j-1)) factor);
        div := !div + factor;
      end
    done;
  done;

  let _ = match !div with
     0 -> div := 1; plus := 128;
    |x when x < 0 -> div := -(!div); plus := 255;
    |x -> div := !div;
  in

  acu := annex3 !acu !div !plus;
  while ((final !acu) <> !acu) do
    acu := final !acu;
  done;
  !acu

let filter matrice img dst =
  let (h, w) = Utile.get_dims img in
  for i = 0 to h-1 do
  for j = 0 to w-1 do
    Sdlvideo.put_pixel_color dst i j (apply_mat matrice i j img);
  done
done

(*let alone img dst =
  let (w,h) = Utile.get_dims img and moy = ref 0 and div = ref 0 in
  for i = 0 to w-1 do
  for j = 0 to h-1 do
    for a = -1 to 1 do
    for b = -1 to 1 do
      if (is_in_bounds (i+a) (j+b) img) then
        moy := !moy + (Sdlvideo.get_pixel_color img (i+a) (j+b));
        div := !div + 1;
    done
    done
    if (moy < 150
  done
  done*)
  

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