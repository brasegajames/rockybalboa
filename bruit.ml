(* pas sur que ça marche *)

let to_grey img =
	let (w, h) = utile.get_dims img in
		for i = 0 to w-1 do
			for j = 0 to h-1 do
				let c = (utile.level (Sdlvideo.get_color img i j)).color in
					Sdlvideo.put_pixel_color img i j c
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