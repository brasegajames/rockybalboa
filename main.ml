let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end

let rec wait_key () =
  let e = Sdlevent.wait_event () in
    match e with
    Sdlevent.KEYDOWN _ -> ()
      | _ -> wait_key ()

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
    let (w,h) = Utile.get_dims img in
    (* On crée la surface d'affichage en doublebuffering *)
    let display = Sdlvideo.set_video_mode w h [`DOUBLEBUF] in
    let new_img = Sdlvideo.create_RGB_surface_format img [] w h in
      (* on affiche l'image *)
      Utile.show img display;
      (* on attend une touche *)
      wait_key ();
      Binarization.image2grey img new_img;
      Utile.show new_img display;
      wait_key ();
      (* 
      Bruit.flou_gaussien new_img new_img;
      Utile.show new_img display;
      wait_key ();
      *)
      Bruit.flou_test new_img new_img;
      Utile.show new_img display;
      wait_key ();
      Binarization.black_and_white new_img;
      Utile.show new_img display;
      wait_key();
      Utile.show img display;
      wait_key();
     (* on quitte *)
      exit 0
  end

let _ = main ()