(*
* http://blog.developpez.com/ocamlblog/p8477/informatique/title_157
*
* http://sophie.aero.jussieu.fr/distrib/Fedora/13/i386/by-pkgid/01d985b39531834b302585eb3505cdb5/files/110
*
*
*)


(* file: menu_entry.ml *)
open GdkKeysyms 

let _ = GtkMain.Main.init ()


(* Pour ouvrir un nouveau fichier ??? *)
let file_dialog ~title ~callback ?filename filter () =
  let sel =
    GWindow.file_selection ~title ~modal:true ?filename () in
    sel#complete filter ;
    sel#cancel_button#connect#clicked ~callback:sel#destroy;
    sel#ok_button#connect#clicked ~callback:
      begin fun () -> let name = sel#filename in
      sel#destroy ();
      callback name
    end;
  sel#show ()


let identity x = x

let input_channel b ic =
  let buf = String.create 1024 and len = ref 0 in
  while len := input ic buf 0 1024; !len > 0 do
    Buffer.add_substring b buf 0 !len
  done

let with_file name ~f =
let ic = open_in name in
  try 
    f ic; 
    close_in ic 
  with 
    exn -> close_in ic; 
    raise exn

    (*  PLEIN DE PETITES FONCTIONS A LA CON POUR PLUS TARD *)
    (* Convertit une chaine en liste de caractere *)

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
    exp (String.length s - 1) []


(*  get a numerical value in a string *)

let get_val s nb =
  try
    let i = ref 0 and k = ref 0 and j = ref 0 and len = ref 0 in
      begin
  while !j != nb do
    if s.[!i] = ' ' then
      j := !j + 1;
    i := !i + 1;
  done;
  k := !i;
  while !i < String.length s && s.[!i] != ' ' do
    flush stdout;
    i := !i + 1;
    len := !len + 1;
  done;
  int_of_string (String.sub s !k !len);
      end
  with
    | _ -> Printf.printf "Echec get_val de %i dans [%s]\n" nb s;
  flush stdout;
  42;;

(* *)

let approx value ecart moy =
  value <= (ecart + moy) && value >= (moy - ecart);;


(* Verifie Que Le Fichier est de l'extension qu'on a demandé *)

let extension = function
    e::_ -> if (e == 'j') || (e == 'b') || (e == 'g') || (e == 'p')
      then true
      else false
  | _ -> false

let rec extension_verif ext l = match (ext,l) with
    ([],[]) -> true
  | ([],e::_) -> false
  | (e::_,[]) -> false
  | (e1::l1,e2::l2) when e1 = e2 -> extension_verif l1 l2
  | (e1::_,e2::_) when e1 <> e2 -> false
  | _ -> false

let verif_file s ext =
  let rec list = explode s and verif = function
      [] -> false
    | e::l when e = '.' -> (match ext with
    "img" -> extension l
  | "txt" -> extension_verif (explode "txt") l
  | _ -> false
      )
    | _::l -> verif l
  in
    verif list


(* print a reduction *)

let print_char entree =
  begin
    for y = 0 to 15 do
      begin
  for x = 0 to 15 do
          Printf.printf "%i" entree.(x).(y);
  done;
  Printf.printf "\n";
      end;
    done;
    Printf.printf "\n\n";
    flush stdout;
  end;;





(* Class option et doc*)

  class options_document = object
val mutable num_filtre = 2 (* COmbien on aura de filtres ? Désgignera le filtre ? *)
val mutable t_doc = 5 (* JESAISPASLOLOLOLOLOLOL *)
val mutable nom_profil = "aucun" (* Nom du filtre sélectionné ? *)

method filtre name () = num_filtre <- name;
  Printf.printf "Filtre n°%d\n" num_filtre;

method doc name () = t_doc <- name;
  Printf.printf "Type de document n°%d\n" t_doc;

method profil entry () = nom_profil <- entry;

method return_filtre () = num_filtre

method return_doc () = t_doc

method return_profil () = nom_profil
end

let opt_doc = new options_document

let make_menu_item ~label ~packing ~callback =
   let item = GMenu.menu_item ~label ~packing () in
     ignore (item#connect#activate ~callback)




(* Menu option *)

let option () = 
  let window1 = GWindow.window ~title:"Options" ~border_width:20 () in
    window1#event#connect#delete ~callback:(fun _ -> window1#destroy (); true);

  let box1 = GPack.vbox ~packing:window1#add () in
  let frame = GBin.frame ~label:"Menu options" ~packing:box1#add () in
    GMisc.label ~text:"Voici le menu des options du projet OCR - RockyBalboa" ~packing:frame#add ();

  let box2 = GPack.hbox ~border_width:30 ~packing:box1#add () in
  let _ = GMisc.label ~text:"Type de filtre utilisé (filtre gaussien par défaut)" ~packing:box2#add () in
  let opt = GMenu.option_menu ~packing:box2#add () in
  let menu = GMenu.menu ~packing:opt#set_menu () in
  let box3 = GPack.hbox ~border_width:30 ~packing:box1#add () in
  let _ = GMisc.separator `HORIZONTAL ~packing:box3#add () in
  let filter (label, filtre) = make_menu_item ~label ~packing:menu#append ~callback:(opt_doc#filtre filtre) in
    List.iter filter [("filtre1", 1); ("filtre2", 2); ("filtre3", 3); ("filtre4", 4)];
 let button = GButton.button ~label:"Quitter" ~packing:box1#add () in
    button#connect#clicked ~callback:(fun () -> window1#destroy ());
    window1#show ()




(*-----------------------------------------------------------------------*)
(* --- Ouverture d'un fichier et chargement du texte dans une chaine --- *)
(*-----------------------------------------------------------------------*)

 class input_buffer n = object
   val mutable s = String.create n
   val mutable pos = 0
   method clear = pos <- 0
   method input f =
    if String.length s < pos + n then begin
      let s' = String.create (String.length s * 2) in
      StringLabels.blit ~src:s ~dst:s' ~src_pos:0 ~dst_pos:0 ~len:pos;
      s <- s'
    end;
     let len = f s pos (String.length s - pos) in
    pos <- pos + len;
    len
   method get =
    StringLabels.sub s ~pos:0 ~len:pos
 end


 let f_to_string n =
   let ic = open_in_bin n in
   let ib = new input_buffer 1024 in
     begin try
   while ib#input (input ic) > 0 do () done;
       with _ -> ()
     end;
     close_in ic; ib#get




(*----------------------------*)
(* ---------- Aide ---------- *)
(*----------------------------*)

 let help () =
   let window2 = GWindow.window ~title:"Aide du logiciel" ~border_width:20 ~width:580 ~height:600 () in
     window2#event#connect#delete ~callback:(fun _ -> window2#destroy ();true);
     let box1 = GPack.vbox ~packing:window2#add () in
     let frame = GBin.frame ~label:"Menu d'aide" ~packing:box1#add () in
       GMisc.label ~text:"Voici le menu d'aide du projet OCR - RockyBalboa" ~packing:frame#add ();
       let textaide = GBin.scrolled_window ~border_width:2 ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC 
                      ~width:580 ~height:500 ~packing:(box1#add) () in
       let b = GText.buffer () in
       let s = f_to_string "aide/aide.txt" in (* NE PAS OUBLIER D'AJOUTER UN FICHIER aide.txt *)
        b#set_text s;
        GText.view ~buffer:b ~cursor_visible:false ~editable:false ~packing:textaide#add_with_viewport ();
        window2#show ()




(*------------------------*)
(* ------ A propos ------ *)
(*------------------------*)

 let apropos () =
   let window3 = GWindow.window ~title:"A propos du logiciel" ~border_width:20 ~width:580 ~height:600 () in
     window3#event#connect#delete ~callback:(fun _ -> window3#destroy (); true);
     let box1 = GPack.vbox ~packing:window3#add () in
     let frame = GBin.frame ~label:"Menu d'a propos" ~packing:box1#add () in
       GMisc.label ~text:"Voici le menu d'a propos du projet OCR - RockyBalboa" ~packing:frame#add ();
       let textapropos = GBin.scrolled_window ~border_width:2 ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC 
                          ~width:580 ~height:500 ~packing:(box1#add) () in
       let b = GText.buffer () in
       let s = f_to_string "aide/apropos.txt" in (* NE PAS OUBLIER D'AJOUTER UN FICHIER apropos.txt *)
         b#set_text s;
         GText.view ~buffer:b ~cursor_visible:false ~editable:false ~packing:textapropos#add_with_viewport ();

       window3#show ()




(*----------------------------*)
(* ---- Type de document ---- *)
(*----------------------------*)

 let document () =
   let window4 = GWindow.window ~title:"Type de document à traiter" ~border_width:20 () in
     window4#event#connect#delete ~callback:(fun _ -> window4#destroy (); true);

  let box1 = GPack.vbox ~packing:window4#add () in
  let frame = GBin.frame ~label:"Choix du document" ~packing:box1#add () in
    GMisc.label ~text:"Choisissez le type de document que le projet OCR - RockyBalboa doit analyser" ~packing:frame#add ();

  let box2 = GPack.hbox ~border_width:30 ~packing:box1#add () in
  let _ = GMisc.label ~text:"Type de document (par défaut tout)" ~packing:box2#add () in
  let opt = GMenu.option_menu ~packing:box2#add () in
  let menu = GMenu.menu ~packing:opt#set_menu () in
  let box3 = GPack.hbox ~border_width:30 ~packing:box1#add () in
  let _ = GMisc.separator `HORIZONTAL ~packing:box3#add () in

    let filter (label, filtre) =
      make_menu_item ~label ~packing:menu#append
  ~callback:(opt_doc#doc filtre) in   (* LES DIFFERENTS TYPES DE DOCS POUR FACILITER *)
      List.iter filter [("Texte seul", 1); ("Texte + image", 2); ("Colonnes", 3); ("Texte + tableau", 4); ("Tout", 5)]; 

 let button = GButton.button ~label:"Quitter" ~packing:box1#add () in
    button#connect#clicked ~callback:(fun () -> window4#destroy ());
    window4#show ()




(*-------------------*)
(* ---- Boutons ---- *)
(*-------------------*)

let xpm_label_box ~file ~text ~packing () =
  if not (Sys.file_exists file) then failwith (file ^ " does not exist");

  let box = GPack.hbox ~border_width:2 ~packing () in
  let pixmap = GDraw.pixmap_from_xpm ~file () in
  GMisc.pixmap pixmap ~packing:(box#pack ~padding:3) ();
  GMisc.label ~text ~packing:(box#pack ~padding:3) ()

let bouton image str titre =
  let window5 = GWindow.window ~title:titre ~border_width:10 () in
    window5#event#connect#delete ~callback:(fun _ -> window5#destroy (); true);

    let button = GButton.button ~packing:window5#add () in
      button#connect#clicked ~callback:(fun () -> window5#destroy ());
      xpm_label_box ~file:image ~text:str ~packing:button#add ();
      window5#show ()




(*-------------------------------------*)
(* --- Fenêtre de sélection de nom --- *)
(*-------------------------------------*)

let choix_nom titre txt entry ~callback =
  let window6 = GWindow.window ~title:titre ~border_width:20 () in
    window6#event#connect#delete ~callback:(fun _ -> window6#destroy (); true);
    let box1 = GPack.vbox ~packing:window6#add () in
    let _ = GMisc.label ~text:txt ~packing:box1#add () in
    let entry = GEdit.entry ~text:entry ~max_length:500 ~packing:box1#add () in
      entry#connect#activate ~callback:(opt_doc#profil entry#text);

      let button = GButton.button ~label:"Ok" ~packing:box1#add () in 
      button#connect#clicked ~callback:(fun () -> (opt_doc#profil entry#text ();
                                                  callback ();
                                                  window6#destroy ()));
      window6#show ();




(*----------------------------------------------*)
(*----------------------------------------------*)
(* ------ Classe : interface OCaml + GTK ------ *)
(*----------------------------------------------*)
(*----------------------------------------------*)
class interface vbox av ?packing ?show () = 
  object (self)
  val text = GText.view ?packing ?show ()
  val buffer = GText.buffer ()
  val mutable filename = None
  val mutable fichier_img = "aucun"
  val mutable img_app = "aucun"
  val mutable txt_app = "aucun"
  val mutable profil_app = "aucun"
  val mutable trait = 0
  val mutable extract = 0
  val mutable export = 0
  (* val mutable matrice = Transfert.pretreatment " " 1      (* JE SAIS PAAAAAS *)
  val mutable reseau = new Neurones.neural_network; *)
  val pack = GPack.notebook ~tab_pos:`TOP ~width:800 ~height:550 ~packing:vbox#add ()
  val pbar = GRange.progress_bar ~packing:av#add ()

  method init () =
    begin
      let label = GMisc.label ~text:"Bienvenue !" () in
  GMisc.image ~file:"img/logo.png" (); (* NE PAS OUBLIER LOGO DE BIENVENUE logo.png *) (* a jarté : ~packing:(pack#append_page ~tab_label:label#coerce) *)
  pbar#set_text "Bienvenue !";
    end

  method text = text

  method return_img () = fichier_img  

  method save_dialog () =
    file_dialog ~title:"Sauvegarder le fichier HTML" ?filename
      ~callback:(fun file -> self#output ~file) "" ();

  method onglet texte img =
   let label = GMisc.label ~text:texte () in
   let scrolled_window = GBin.scrolled_window
     ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC () in
   let _ = GMisc.image ~file:img ~packing:scrolled_window#add_with_viewport () in
     ();




(*----------------------------------*)
(* ---- Chargement d'une image ---- *)
(*----------------------------------*)

  method open_image () =
    file_dialog ~title:"Charger une image" ~callback:self#load_image "*.jpg" ();

  method load_image name =
    if not (verif_file name "img") then
      bouton "img/jpg.gif" "Le fichier n'est pas une image" "Erreur lors du chargement"
    else
      begin
  if trait = 1 then Gc.full_major ();
  self#affichage 100 "Image chargée";
  pack#remove_page pack#current_page;
  pack#remove_page pack#current_page;
  pack#remove_page pack#current_page;
  pack#remove_page pack#current_page;
  self#onglet "Image à utiliser" name;
  fichier_img <- name;
  trait <- 0;
  extract <- 0;
  export <- 0
      end




(*------------------------------*)
(* ------ Menu d'options ------ *)
(*------------------------------*)

  method options () = option ();


(*---------------------------*)
(* ------ Menu d'aide ------ *)
(*---------------------------*)


  method help () = help ();


(*-------------------------------*)
(* ------ Menu d'a propos ------ *)
(*-------------------------------*)

  method apropos () = apropos ();


(*---------------------------------*)
(* ------ Choix du document ------ *)
(*---------------------------------*)

  method doc () = document ();


(*--------------------------*)
(* ------ Avancement ------ *)
(*--------------------------*)
  method affichage pourcentage txt_av =
    begin
      pbar#set_text (txt_av ^ " : " ^ string_of_int pourcentage ^ "%");
      pbar#set_fraction (float_of_int pourcentage /. 100.);
      ();
    end


(*-----------------------------------------*)
(* ------ Extraction des caractères ------ *)
(*-----------------------------------------*)

  method extraction () =
    begin
    if fichier_img <> "aucun" then
      begin
        if export = 1 then 
          pack#remove_page 3;
        export <- 0;
        if extract = 1 then 
          pack#remove_page 2;
        if trait = 0 then 
          self#traiting ();
        extract <- 1;
        (* -- Extraction -- *)  
    (* EXTRACTION PAS ENCORE FAIIIIIITE
        Extraction.fn_extraction matrice (opt_doc#return_doc ()) self;
    *)
        self#onglet "Visualisation" "tmp/ocr_img_visu.ppm";
        pack#goto_page 2; 
        
      end
    else
      bouton "img/check.gif" "Image non chargée" "Erreur" (* NE PAS OUBLIER D'AJOUTER check.gif*)
    end




(*--------------------------*)
(* ---- Apprentissage  ---- *)
(*--------------------------*)

  method load_img_app name =
    if not (verif_file name "img") then
      begin
  bouton "img/jpg.gif" "Le fichier n'est pas une image" "Erreur lors du chargement"; (* NE PAS OUBLIER D'AJOUTER jpg.gif *)
  img_app <- "aucun";
  self#affichage 0 "Impossible de créer le profil d'apprentissage";
      end
    else
      begin
  img_app <- name;
  file_dialog ~title:"Charger le texte d'apprentissage" ~callback:self#load_txt_app "*.txt" ()
      end

  method load_txt_app name =
    if not (verif_file name "txt") then
      begin
  bouton "img/txt.gif" "Le fichier n'est pas un fichier texte" "Erreur lors du chargement"; (* NE PAS OUBLIER D'AJOUTER txt.gif *)
  txt_app <- "aucun";
  self#affichage 0 "Impossible de créer le profil d'apprentissage";
      end
    else
      begin
  txt_app <- name;
  self#select_name_profil ();
  ();
      end

  method select_name_profil () = choix_nom "Choix du nom du profil d'apprentissage" (* peut être remettre à la suite ? *)
                                "Choisissez le nom du profil d'apprentissage" "tapez le nom" ~callback:self#apprentissage;

  method apprentissage () =
    begin
      if txt_app <> "aucun" && img_app <> "aucun" then
  begin
    fichier_img <- img_app;
    self#extraction ();
    (* On fait notre apprentissage *)
(* CAY PAS FAIT LE RESEAAAAU ET APPRENTISSAGE
    reseau#init;
    Learn.learn_page txt_app matrice reseau;
    reseau#save (opt_doc#return_profil ());
*)
    profil_app <- "profile/"^(opt_doc#return_profil ())^".txt";
    self#affichage 100 "Profil d'apprentissage créé";
  end
    end

  method apprentissage_interface () = file_dialog ~title:"Charger l'image d'apprentissage" ~callback:self#load_img_app "*.jpg" ();

  method load_profil name =
    if not (verif_file name "txt") then
      bouton "img/txt.gif" "Le fichier n'est pas un fichier de profil" "Erreur lors du chargement"
    else
  profil_app <- name;

  method profil_app () = file_dialog ~title:"Charger un profil d'apprentissage" ~callback:self#load_profil "*.txt" ();




(*---------------------------------------------*)
(* ------ Reconnaissance des caractères ------ *)
(*---------------------------------------------*)

  method exportation () =
    begin
      if export = 1 then 
        pack#remove_page 3;
      export <- 1;
      let label = GMisc.label ~text:"Votre texte :p" () in
      let scrolled_window = GBin.scrolled_window
  ~hpolicy:`AUTOMATIC ~vpolicy:`AUTOMATIC () in
      let s = f_to_string "tmp/exportation.html" in
  buffer#set_text s;
  let txt = GText.view ~buffer:buffer ~cursor_visible:true ~editable:true ~packing:scrolled_window#add_with_viewport () in
    GtkSpell.attach ~lang:"fr iso8859-1" txt;
  pack#goto_page 3;
  bouton "img/check.gif" "Les images découpées se trouvent dans le dossier \"extract\"" "Information";
    end

  method reconnaissance () =
    begin
      if extract = 0 || trait = 0 then
  begin
    if fichier_img <> "aucun" then
      begin
        if extract = 0 then self#extraction ();
        if trait = 0 then self#traiting ();
        if profil_app = "aucun" then
    bouton "img/supprimer.gif" "Aucun profil d'apprentissage n'est chargé" "Erreur de reconnaissance" (* NE PAS OUBLIER supprimer.gif*)
        else
    begin
      (* On fait notre reconnaissance avec le profil *)
  (* ENCORE MANQUE DE RESEAU + MISE EN FORME POUR FICHIER HTML 
      reseau#init;
      reseau#load profil_app;
      Miseenforme.exportation_html reseau matrice; 
  *)
      self#affichage 100 "Reconnaissance terminée";
      self#exportation ();
    end
      end
    else
      bouton "img/check.gif" "Image non chargée" "Erreur lors du chargement";
  end
      else
  begin
    if profil_app = "aucun" then
      bouton "img/supprimer.gif" "Aucun profil d'apprentissage n'est chargé" "Erreur de reconnaissance"
    else
      begin
        (* On fait notre reconnaissance avec le profil *)
    (* ENCORE MANQUE DE RESEAU + MISE EN FORME POUR FICHIER HTML
        reseau#init;
        reseau#load profil_app;
        Miseenforme.exportation_html reseau matrice;
    *)
        self#affichage 100 "Reconnaissance terminée";
        self#exportation ();
      end
  end
    end




(*------------------------------------*)
(* ---- Methode de prétraitement ---- *)
(*------------------------------------*)

  method traiting () =
    begin
      if fichier_img <> "aucun" then
  begin
    if export = 1 then pack#remove_page 3;
    if extract = 1 then pack#remove_page 2;
    export <- 0;
    extract <- 0;
    if trait = 1 then
      begin
        pack#remove_page 1;
        Gc.full_major ();
      end;
  (* WHAT IZ TEH MATRICE ?
    matrice <- Transfert.pretreatment fichier_img (opt_doc#return_filtre ());
  *)
    self#affichage 100 "Pré-traitement terminé";
    trait <- 1;
    self#onglet "Pré-traitement" "tmp/ocr_img.pgm";
    pack#goto_page 1;
  end
      else
  bouton "img/check.gif" "Image non chargée" "Erreur lors du chargement"
    end




  (*------------------------------------------*)
  (* ---- Autres (futures améliorations) ---- *)
  (*------------------------------------------*)

  method save_file () =
    match filename with
      Some file -> self#output ~file
    | None -> self#save_dialog ()

  method output ~file =
    try
      if Sys.file_exists file then 
        Sys.rename file (file ^ "~");
      let s = buffer#get_text () in
      let oc = open_out file in
      output_string oc (Glib.Convert.locale_from_utf8 s);
      close_out oc;
      filename <- Some file
    with 
      _ -> prerr_endline "Save failed"
end

let window = GWindow.window ~width:800 ~height:600 ~title:"OCR - RockyBalboa" ()
let vbox = GPack.vbox ~packing:window#add ()
let menubar = GMenu.menu_bar ~packing:vbox#pack ()
let factory = new GMenu.factory ~accel_path:"<INTERFACE>/" menubar (* BAR EN HAUT AVEC TOUS LES MENUS, *)
let accel_group = factory#accel_group                               (* ON LES AJOUTE TOUS APRES. *)
let file_menu = factory#add_submenu "Fichier"
let pref_menu = factory#add_submenu "Préférences"
let app_menu = factory#add_submenu "Apprentissage"
let edit_menu = factory#add_submenu "Reconnaissance"
let help_menu = factory#add_submenu "Aide"
let vbox_av = GPack.vbox ~packing:vbox#add ()
let interface = new interface vbox vbox_av ()

let _ =
  interface#init ();
  window#connect#destroy ~callback:GMain.quit;
  let factory = new GMenu.factory ~accel_path:"<INTERFACE File>/////" file_menu ~accel_group
  in
    factory#add_item "Charger une image" ~key:_O ~callback:interface#open_image;
    factory#add_item "Sauvegarder" ~key:_S ~callback:interface#save_file;
    factory#add_item "Sauvegarder sous ..." ~callback:interface#save_dialog;
    factory#add_separator ();
    factory#add_item "Quitter" ~key:_Q ~callback:window#destroy;
    window#add_accel_group accel_group;
    let factory = new GMenu.factory ~accel_path:"<INTERFACE File>/////" app_menu ~accel_group
    in
      factory#add_item "Charger un profil" ~callback:interface#profil_app;
      factory#add_item "Apprendre à reconnaitre les formes" ~key:_F ~callback:interface#apprentissage_interface;
      let factory = new GMenu.factory ~accel_path:"<INTERFACE File>/////" edit_menu ~accel_group
      in
  factory#add_item "Pré-traitement" ~key:_P ~callback:interface#traiting;
  factory#add_item "Extraction des caractères" ~key:_E ~callback:interface#extraction;
  factory#add_item "Reconnaissance des caractères" ~key:_R ~callback:interface#reconnaissance;
  let factory = new GMenu.factory ~accel_path:"<INTERFACE File>/////" pref_menu ~accel_group
  in
    factory#add_item "Type de document" ~key:_T ~callback:interface#doc;
    factory#add_item "Options" ~key:_I ~callback:interface#options;
    let factory = new GMenu.factory ~accel_path:"<INTERFACE File>/////" help_menu ~accel_group
    in
      factory#add_item "Aide" ~key:_H ~callback:interface#help;
      factory#add_item "A propos" ~key:_A ~callback:interface#apropos;

      interface#text#event#connect#button_press
        ~callback:(fun ev ->
    let button = GdkEvent.Button.button ev in
      if button = 3 then begin
        file_menu#popup ~button ~time:(GdkEvent.Button.time ev); true
      end else false);
      window#show ();
      let () = GtkData.AccelMap.load "test.accel" in
        GMain.main ()









(* 
let print msg () =
  print_endline msg;
  flush stdout

let print_toggle selected =
  if selected
  then print_endline "On"
  else print_endline "Off";
  flush stdout

let print_selected n selected =
  if selected then (
    print_endline (string_of_int n);
    flush stdout
  )

let nouv = 
  print "Nouveau";
  Printexc.print main

let file_entries = [
  `I ("New", nouv );
  `I ("Open", print "Open");
  `I ("Save", print "Save");
  `I ("Save As", print "Save As");
  `S;
  `I ("Quit", GMain.Main.quit)
]

let option_entries = [
  `C ("Check", false, print_toggle);
  `S;
  `R [("Rad1", true, print_selected 1);
      ("Rad2", false, print_selected 2);
      ("Rad3", false, print_selected 3)]
]

let help_entries = [
  `I ("About", print "About");
]

let entries = [
  `M ("File", file_entries);
  `M ("Options", option_entries);
  `M ("Help", help_entries)
]

let create_menu label menubar =
  let item = GMenu.menu_item ~label ~packing:menubar#append () in
  GMenu.menu ~packing:item#set_submenu ()

let main () =
  (* Make a window *)
  let window = GWindow.window ~title:"Menu Entry" ~border_width:10 () in
  window#connect#destroy ~callback:GMain.Main.quit;
  
  let main_vbox = GPack.vbox ~packing:window#add () in

  let menubar = GMenu.menu_bar ~packing:main_vbox#add () in

  let menu = create_menu "File" menubar in
  GToolbox.build_menu menu ~entries:file_entries;

  let menu = create_menu "Options" menubar in
  GToolbox.build_menu menu ~entries:option_entries;

  let menu = create_menu "Help" menubar in
  GToolbox.build_menu menu ~entries:help_entries;

  (* Popup menu *)
  let button = GButton.button ~label:"Popup" ~packing:main_vbox#add () in
  button#connect#clicked ~callback:(fun () ->
    GToolbox.popup_menu ~entries ~button:0
	  ~time:(GtkMain.Main.get_current_event_time ())
    );

  window#show ();
  GMain.Main.main ()

let _ = Printexc.print main () *)