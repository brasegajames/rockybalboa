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
 
(* Fonctions du tp *)

let level (r,g,b) = 0.3 *. (float_of_int r) +. 0.59 *. (float_of_int g) +. 0.11 *. (float_of_int b)

let color2grey (r,g,b) = 
  (int_of_float (level (r,g,b)), int_of_float (level (r,g,b)), int_of_float (level (r,g,b)))

let image2grey src dst = 
 let w = (Sdlvideo.surface_info src).Sdlvideo.w in
 let h = (Sdlvideo.surface_info src).Sdlvideo.h in
  for i = 0 to w-1 do
    for j = 0 to h-1 do
    Sdlvideo.put_pixel_color dst i j (color2grey (Sdlvideo.get_pixel_color src i j))
    done
  done

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
    let new_img = Sdlvideo.create_RGB_surface_format img [] w h in
      (* on affiche l'image de départ *)
      show img display;
      wait_key ();
      (* on grise l'image *)
      image2grey img new_img;
      (* on affiche la nouvelle image *)
      show new_img display;
      (* on attend une touche *)
      wait_key ();
      (* on quitte *)
      exit 0
  end
 
let _ = main ()