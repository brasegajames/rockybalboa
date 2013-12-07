let bsnumb = ref 0

(* Module for file opening and saving text*)
module Aux =
    struct
        let load (text : GText.view) file =
        let ich = open_in file in
        let len = in_channel_length ich in
        let buf = Buffer.create len in
            Buffer.add_channel buf ich len;
            close_in ich;
            text#buffer#set_text (Buffer.contents buf)

        let save (text : GText.view) file =
        let och = open_out file in
            output_string och (text#buffer#get_text ());
            close_out och

         let loadimage (name:bool) (image : GMisc.image) file =
            Sdlvideo.save_BMP (Sdlloader.load_image file) "thumbs/temp.bmp";
            let buf = GdkPixbuf.from_file_at_size file ~width:520 ~height:700 in
                image#set_pixbuf buf;
            print_endline file;
            if name then
            let aux file =
            match file with
                |f when f.[(String.length f)-5]='1'||f.[(String.length f)-9]='1'->begin bsnumb := 1 end
                |f when f.[(String.length f)-5]='2'||f.[(String.length f)-9]='2'->begin bsnumb := 2 end
                |f when f.[(String.length f)-5]='3'||f.[(String.length f)-9]='3'->begin bsnumb := 3 end
                |f ->begin bsnumb := 0 end
            in
            aux file;
            print_endline (string_of_int(!bsnumb))

    end

(* Init of GTK *)
let _ = GMain.init ()

(* Main window *)
let window_Main =
        GWindow.window
                ~width:1000
                ~height:800
                ~resizable:false
                ~title:"CrapOCR" ()

(* Main box for the menu and the main box *)
let box_Menu =
        GPack.vbox
                ~packing:window_Main#add ()

(* Menu bar *)
let bar_Menu =
        let menubar =
                GMenu.menu_bar
                        ~packing:box_Menu#pack ()
        in
                menubar

(* Main box *)
let box_Main =
        GPack.hbox
                ~packing:box_Menu#add ()

(* Selection File dialog *)
let action stock event action =
        let dialog =
                GWindow.file_chooser_dialog
                        ~action:`OPEN
                        ~parent:window_Main
                        ~destroy_with_parent:true ()
        in
                dialog#add_button_stock `CANCEL `CANCEL;
                dialog#add_select_button_stock stock event;
        let fun_action () =
                if dialog#run () = `OPEN then
                        Gaux.may action dialog#filename;
                        dialog#misc#hide ()
        in
                fun_action ()

(* Confirmation dialog *)
let confirm () =
  let dialog =
                        GWindow.message_dialog
                    ~message:"<b><big>Do you really want to quit?</big>\n\n\
                                                Warning :\nAny unsaved data will be lost.</b>\n"
                    ~parent:window_Main
                    ~destroy_with_parent:true
                    ~use_markup:true
                    ~message_type:`QUESTION
                    ~position:`CENTER_ON_PARENT
                    ~buttons:GWindow.Buttons.yes_no ()
        in
  let temp =
                dialog#run () = `NO
        in
          dialog#destroy ();
          temp

(* Image box and image displayer *)
let box_Image =
        GPack.hbox
                ~spacing:5
        ~width:200
                ~border_width:5
                ~homogeneous:true
                ~packing:box_Main#add ()

let image =
    let buf = GdkPixbuf.create ~width:520 ~height:700 () in
        GMisc.image
        ~pixbuf:buf
                ~packing:box_Image#add ()

(* Text Box and text buffer*)
let box_Text =
        GPack.hbox
                ~spacing:5
                ~border_width:5
                ~packing:box_Main#add()

let tag_tab = GText.tag_table ()
let buffer = GText.buffer
    ~tag_table:tag_tab
    ~text:"The text will be extracted here" ()

let text =
        let scroll =
                GBin.scrolled_window
                        ~hpolicy:`ALWAYS
                        ~vpolicy:`ALWAYS
                        ~shadow_type:`ETCHED_IN
                        ~packing:box_Text#add ()
        in
    let basetext = "" in
    buffer#set_text(basetext);
        let textaux =
                GText.view
            ~buffer:buffer
                        ~packing:scroll#add ()
        in
        GtkSpell.attach ~lang:"en_US" textaux;
                textaux#misc#modify_font_by_name "Arial 10";
                textaux

(* Function to open an image and save a text *)
let action_open () =
        action `OPEN `OPEN (Aux.loadimage true image)

let action_save () =
        action `SAVE `SAVE (Aux.save text)

(* Copyright dialog *)
let about () =
    let dialog =
                GWindow.about_dialog
             ~authors:["Underflow";"Damwdan";"Yoone";"Ciremya"]
             ~copyright:"Copyright © 2012 Error 418"
             ~license:"GNU General Public License v3.0"
             ~version:"Beta"
             ~website:"http://ocr.yoone.eu"
             ~website_label:"OCR 418"
             ~position:`CENTER_ON_PARENT
             ~parent:window_Main
             ~destroy_with_parent:true ()
    in
         let fun_about () =
                ignore (dialog#run ());
                dialog#misc#hide ();
        in
                fun_about ()

(* Function to quit *)
let quit () =
        if not (confirm ()) then
                GMain.quit ()

(* Image Processing *)
let processing funct imagepath =
    funct imagepath;
    Aux.loadimage false image ("thumbs/temp.bmp")

let processonly funct imagepath =
    funct imagepath

let greyscale () =
    processing (Main.greyscale) ("thumbs/temp.bmp")

let binarize () =
    processing (Main.binarize) ("thumbs/temp.bmp")

let rotate () =
    processing (Main.rotate) ("thumbs/temp.bmp")

let median () =
    processing (Main.median) ("thumbs/temp.bmp")

let convolution () =
    processing (Main.convolution) ("thumbs/temp.bmp")

let segmentation () =
    processing (Main.segmentation) ("thumbs/temp.bmp")

let skeleton () =
    processing (Main.hilditch) ("thumbs/temp.bmp")

let detection () =
    processing (Main.detection) ("thumbs/temp.bmp")

let fulldisplay () =
    processing (Main.fulldisplay) ("thumbs/temp.bmp")

let applyall () =
    processonly (Main.convolution) ("thumbs/temp.bmp");
    processonly (Main.binarize) ("thumbs/temp.bmp");
    processing (Main.rotate) ("thumbs/temp.bmp");
    processonly (Main.segmentation) ("thumbs/temp.bmp");
    processonly (Main.hilditch) ("thumbs/temp.bmp");
    processing (Main.detection) ("thumbs/temp.bmp")

let extraction () =
    let s = Main.extraction !bsnumb in
        Aux.load text s

let checkspell () =
    GtkSpell.recheck_all text

let xor_approx () =
    Nn.xor_sigmoid_test ();
    print_newline ()

(* Submenus *)
let fact =
        let factory =
                new GMenu.factory bar_Menu
        in
                let file_menu =        factory#add_submenu "File" in
                let file =
                        new GMenu.factory file_menu
                in
                        begin (* File Menu*)
                                ignore(file#add_item "Open" ~callback:action_open);
                                ignore(file#add_item "Quit" ~callback:quit);
                        end;
                let process_menu = factory#add_submenu "Image Processing" in
                let process =
                        new GMenu.factory process_menu
                in
                        begin (* Image Processing menu *)
                ignore(process#add_item "Median Filter" ~callback:median);
                ignore(process#add_item "Convolution Filter" ~callback:convolution);
                                ignore(process#add_item "Greyscale" ~callback:greyscale);
                                ignore(process#add_item "Binarization" ~callback:binarize);
                                ignore(process#add_item "Rotation" ~callback:rotate );
                                ignore(process#add_item "Segmentation" ~callback:segmentation);
                                ignore(process#add_item "Make skeleton" ~callback:skeleton);
                                ignore(process#add_item "Detection" ~callback:detection);
                                ignore(process#add_item "Exctraction" ~callback:extraction);
                                ignore(process#add_item "Apply all" ~callback:applyall);
                        end;
                let text_menu = factory#add_submenu "Text Processing" in
                let text =
                        new GMenu.factory text_menu
                in
                        begin (* Text Processing Menu *)
                                ignore(text#add_item "Export text" ~callback:action_save);
                                ignore(text#add_item "Check spelling" ~callback:checkspell);
                        end;

                let nn_menu = factory#add_submenu "Neural network" in
                let nn =
                    new GMenu.factory nn_menu
                in
                    begin
                        ignore(nn#add_item "Xor approximation" ~callback:xor_approx);
                    end;

                let help_menu = factory#add_submenu "Help" in
                let help =
                        new GMenu.factory help_menu
                in
                        begin (* Help menu *)
                                ignore(help#add_item "Display image" ~callback:fulldisplay);
                                ignore(help#add_item "About" ~callback:about);
                                ignore(help#add_item "Help" ~callback:(fun () ->
                    print_endline "This function was not implemented yet."));
                        end;
                factory

(* Main function *)

let _ =
    Sdl_helper.sdl_init ();
        ignore(window_Main#connect#destroy ~callback:quit);
        window_Main#show ();        
        GMain.main ()

