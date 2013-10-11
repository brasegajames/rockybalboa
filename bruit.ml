(* pas sur que ça marche *)

let to_grey img =
	let (w, h) = Utile.get_dim img in
		for i = 0 to w-1 do
			for j = 0 to h-1 do
				let c = Utile.level (Sdlvideo.get_color img i j) in
					Sdlvideo.put_pixel img i j c
			done
		done

(* 
Gris: quand r=g=b // gris "foncé" quand inf à 180.
On va donc virer ceux étant sup à 180 en les passant en blanc.
 *)

let whiterthanwhite img =
	let (w, h) = Utile.get_dim img in
		for i = 0 to w-1 do
			for j = 0 to h-1 do
				let 