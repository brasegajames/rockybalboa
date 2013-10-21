(*
(* Dimensions d'une image *)
let get_dims img =
  ((Sdlvideo.surface_info img).Sdlvideo.w, (Sdlvideo.surface_info img).Sdlvideo.h)
 
(* init de SDL *)
let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end
 
(* attendre une touche ... *)
let rec wait_key () =
  let e = Sdlevent.wait_event () in
    match e with
    Sdlevent.KEYDOWN _ -> ()
      | _ -> wait_key ()
 
(*
  show img dst
  affiche la surface img sur la surface de destination dst (normalement l'écran)
*)
let show img dst =
  let d = Sdlvideo.display_format img in
    Sdlvideo.blit_surface d dst ();
    Sdlvideo.flip dst
*) 




let level (r,g,b) = ( 0.3 *. float_of_int(r) +. 0.59 *. float_of_int(g) +. 0.11 *. float_of_int(b))



(*let color2grey (r,g,b) = let l = int_of_float(Utile.level (r,g,b)) in
l,l,l  ;;*)

let color2grey (r,g,b) = 
  (int_of_float (level (r,g,b)), int_of_float (level (r,g,b)), int_of_float (level (r,g,b)))

let image2grey src dst = 
 let (w,h) = Utile.get_dims src in
  for i = 0 to w-1 do
    for j = 0 to h-1 do
    Sdlvideo.put_pixel_color dst i j (color2grey (Sdlvideo.get_pixel_color src i j))
    done
  done

(* Renvoi la moyenne du level des pixels de l'image (entre 0 et 1) *)
let averageimage src =
        let acc = ref 0. in
        let (w,h) = Utile.get_dims src in
                for i = 0 to w do      (*boucle de parcour*)
                        for j = 0 to h do
                        acc := !acc +. Utile.level(Sdlvideo.get_pixel_color src i j)
                        done (*garde le lveles des pixel de l image*)
                done;

        (!acc)/.(float_of_int(w)*.float_of_int(h)) (* acc / par le nombre de pixel de l'image*)

(* Passe l'image en noir et blanc, si le level d'un pixel est sous le level moyen ou si il est ou dessus *)
let black_and_white img =
    let moy = averageimage img in
    let (w,h) = Utile.get_dims img in
        for i = 0 to w-1 do
                for j = 0 to h-1 do
                        if Utile.level(Sdlvideo.get_pixel_color img i j) < moy -.0.12
                        then Sdlvideo.put_pixel_color img i j (0,0,0) (*applique noir au pixel*)
                        else Sdlvideo.put_pixel_color img i j (255,255,255)        (*applique la couleur blanche *)
                done
        done

let parasite img dst=
  let (w,h) = Utile.get_dims img in
      for i = 0 to w-1 do
        for j = 0 to h-1 do
          if Sdlvideo.get_pixel_color img i j = (0,0,0) then
            if Sdlvideo.get_pixel_color img (i-1) j = (255,255,255) && Sdlvideo.get_pixel_color img (i+1) j = (255,255,255)
                && Sdlvideo.get_pixel_color img i (j-1) = (255,255,255) && Sdlvideo.get_pixel_color img i (j+1) = (255,255,255)
            then Sdlvideo.put_pixel_color img i j (255,255,255)
        done
      done
      
(*
(* main *)
let main () =
  begin
    (* Nous voulons 1 argument *)
    if Array.length (Sys.argv) < 2 then
      failwith "Il manque le nom du fichier!";
    (* Initialisation de SDL *)
    sdl_init ();
    (* Chargement d'une image *)
    let img = Sdlloader.load_image Sys.argv.(1) in
    (* On récupère les dimensions *)
    let (w,h) = get_dims img in
    (* On crée la surface d'affichage en doublebuffering *)
    let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
      (* on affiche l'image *)
      show img display;
      (* on attend une touche *)
      wait_key ();
      (* on quitte *)
      exit 0
  end
 
let _ = main ()

*)