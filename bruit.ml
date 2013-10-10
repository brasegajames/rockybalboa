(* pas sur que ça marche *)

let to_grey img =
	let (w, h) = Utile.get_dim img in
		for i = 0 to w-1 do
			for j = 0 to h-1 do
				let c = Utile.level (Sdlvideo.get_color img i j) in
					Sdlvideo.pixel_format_video.colorkey c
			done
		done
		
