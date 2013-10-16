(* pas sur que ça marche *)

let to_grey img dst =
	let (w, h) = Utile.get_dims img in
		for i = 0 to w do
			for j = 0 to h do
				let (color) c = Utile.level (Sdlvideo.get_pixel_color img i j).toColor in
				Sdlvideo.put_pixel_color dst i j c
			done
		done

(* 
Gris: quand r=g=b // gris "foncé" quand inf à 180.
On va donc virer ceux étant sup à 180 en les passant en blanc.
 *)

(*
let whiterthanwhite img =
	let (w, h) = utile.get_dim img in
		for i = 0 to w-1 do
			for j = 0 to h-1 do
				let (* jesaispasencore *)
*)
(*
let coupe_mediane img =
	let (h, w) = utile.get_dim img in
		for i = 0 to h do
			for j = 0 to w do
				let matrix = Array.make h w (Sdlvideo.get_pixel img i j) in
				if (i = 0) then
					if (j = 0) then
						
			done
		done
*)	