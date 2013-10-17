(* Des trucs qui peuvent être utiles *)

(*
let make_matrix n1 n2 init =
	Array.init n1 (fun _ -> Array.Make n2 init)

=> Inutile 
val make_matrix : int -> int -> 'a -> 'a array array
*)

let get_dims img =
	((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)

let level (r,g,b) = 
   (0.3 *. float_of_int(r) +. 0.59 *. float_of_int(g) +. 0.11 *. float_of_int(b))/.255.

(* fonctions pour le tri fusion (en: merge_sort) *)

let rec split = function
	| [] -> ([],[])
	| [e] -> ([e],[])
	| (premier::second::reste) -> let (a,b) = split reste in 
                              (premier::a,second::b)

let rec merge liste1 liste2 =
    match liste1,liste2 with
    |[],_->liste2
    |_,[]->liste1
    |t1::q1,t2::q2->
        if t1<t2 then
            t1::(merge q1 liste2)
        else
            t2::(merge liste1 q2)

let rec merge_sort = function
     | [] -> []
     | [e] -> [e]
     | liste -> let (e1,e2) = split liste in
                            merge (merge_sort e1) (merge_sort e2)

(* show img dst : affiche la surface img sur la surface
  de destination dst (normalement l'écran) *)
let show img dst =
  let d = Sdlvideo.display_format img in
    Sdlvideo.blit_surface d dst ();
    Sdlvideo.flip dst

(* Affiche l'image img sur un cadre de la bonne taille *)
let display img =
  let (w, h) = get_dims img in
    show img (Sdlvideo.set_video_mode w h [`DOUBLEBUF])


let createBoolFromImg src = 
  let (w,h) = get_dims src in
    let mat = Array.make_matrix w h false in 
      for i = 0 to w - 1 do
        for j = 0 to h - 1 do
          let (r,_,_) = Sdlvideo.get_pixel_color src i j in
          if r = 0 then
            mat.(i).(j) <- true;
        done;
      done;
      mat


let createImgFromBool src dst =
  let w = Array.length src and h = Array.length src.(0) in
      for i = 0 to w - 1 do
        for j = 0 to h - 1 do
          let c = ref (0, 0, 0) in
            if not(src.(i).(j)) then c := (255, 255, 255);
            Sdlvideo.put_pixel_color dst i j !c;
        done;
      done


(* Dessine un rectangle dans une matrice avec
les coordonnées n de la couleur color *)
let drawRect m n color =
  let (a, b, c, d) = n in
    let j = ref b in
      for i = a to c do
        m.(i).(!j) <- color;
      done;
      j := d;
      for i = a to c do
        m.(i).(!j) <- color;
      done;
      j := a;
      for i = b to d do
        m.(!j).(i) <- color;
      done;
      j := c;
      for i = b to d do
        m.(!j).(i) <- color;
      done
