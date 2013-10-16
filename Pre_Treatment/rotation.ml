(*Rotation et détection d'angle*)

let hough matrice =
    (*Déclaration des constantes*)
    let pi = 3.14159265 and pi2 = 1.57079633 in
    let (w,h) = (Array.length matrice, Array.length matrice.(0)) in
    let diagonal = hypot w h in
    let vote_matrice = Array.make_matrix diagonal ((int_of_float(pi*.100.))+1) 0 
        in
        (* Déclaration des variables teta max et l'angle le plus voté *)
        let teta_max = ref 0. in
        let vote_max = ref 0  in
        (* Parcours de l'image *)
        for y=0 to h-1 do
           for x=0 to w-1 do
              if (matrice.(x).(y) == (0,0,0)) then
                begin
             (* Test l'angle qu'on fera varier de -pi/2 a pi/2 *)
                let t = ref(-.pi02) in
                (* Parcours de l'intervalle d'angle teta*)                   
                 while ( !t <= pi02 ) do
                  let droite =int_of_float((float x *. (cos !t))+.(float
                  y *. (sin !t))) in
                       if droite>=0 then (* on remplis le tableau de vote *)
                         begin
                          let teta_i = int_of_float(!t*.100. +. pi02*.100.)in
                           vote_matrice.(droite).(teta_i) <-
                                  vote_matrice.(droite).(teta_i)+1;
                           if !vote_max < vote_matrice.(droite).(teta_i) then
                                  begin
                                  vote_max := vote_matrice.(droite).(teta_i);
                                  teta_max := !t;
                                  end;
                         end;
                         t := !t +. 0.01; (* On incrémente l'angle de 0.01 *) 
                 done;
                end;
           done;
        done;
        (!teta_max);;


let img2matrice img =
   let(w,h) = Utile.get_dims img in
   let matrice = Array.make_matrix w h (255,255,255) in
    for y=0 to h-1 do
       for x=0 to w-1 do
         matrice.(x).(y) <- Sdlvideo.get_pixel_color img x y
       done;
    done;
     matrice;;
               
