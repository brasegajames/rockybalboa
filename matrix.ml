let create w h e = Array.create_matrix w h e

let iteri2 f m = Array.iteri (fun i a -> Array.iteri f a) m

let iteri f m =
	for i = 0 to Array.length m - 1 do
		for j = 0 to Array.length m.(i) - 1 do
			f i j m.(i).(j);
			(*Printf.printf "%d " m.(i).(j)*)
		done
	done

let mapi f m =
	if Array.length m = 0 then [|[||]|] else
	let m2 = create (Array.length m) (Array.length m.(0)) 0 in
	for i = 0 to Array.length m - 1 do
		for j = 0 to Array.length m.(i) - 1 do
			m2.(i).(j) <- f i j m.(i).(j);
			(*Printf.printf "%d " m2.(i).(j)*)
		done
	done;
	m2

let of_img f img (x,y,w,h) =
   	let matrice = create w h (f (255,255,255)) in
	for i = x to w - 1 do
	   	for j = y to h - 1 do
	    	matrice.(i).(j) <- f (Sdlvideo.get_pixel_color img i j)
	   	done
 	done;
    matrice

let get_dims m = if Array.length m = 0 then (0, 0) else (Array.length m, Array.length m.(0))

let mult x y =
	let x0 = Array.length x
   	and y0 = Array.length y in
   	let y1 = if y0 = 0 then 0 else Array.length y.(0) in
   	let z = Array.make_matrix x0 y1 0. in
   	for i = 0 to x0-1 do
     		for j = 0 to y1-1 do
       			for k = 0 to y0-1 do
         			z.(i).(j) <- z.(i).(j) +. x.(i).(k) *. y.(k).(j)
       			done
     		done
   	done;
   	z