let sdl_init () =
  begin
    Sdl.init [`EVERYTHING];
    Sdlevent.enable_events Sdlevent.all_events_mask;
  end

let keepit file = Sdlloader.load_image file

let pretreatment file filter nb =
    (* Chargement d'une image *)
    let img = Sdlloader.load_image file in
    (* On récupère les dimensions *)
    let (w,h) = Utile.get_dims img in
    (* On crée la surface d'affichage en doublebuffering *)
    let new_img = Sdlvideo.create_RGB_surface_format img [] w h in
      Binarization.image2grey img new_img;
      Bruit.filter filter new_img img;
      Binarization.black_and_white img;
      Binarization.parasite img;
      Sdlvideo.save_BMP img ("tmp/ocr_img"^(string_of_int nb)^".pgm");
      img

let rotation file =
    let img = Sdlloader.load_image file in
    let (w,h) = Utile.get_dims img in
    let new_img = Sdlvideo.create_RGB_surface_format img [] w h in
        Rotation.rotate1 img new_img;
        Sdlvideo.save_BMP new_img "tmp/ocr_rota.pgm"


let xycut file = 
    let img = Sdlloader.load_image file in
    ignore(Xy_cut.get_tree_blocks img 6);
        Sdlvideo.save_BMP img "tmp/ocr_cut.pgm"