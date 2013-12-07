let pi = acos (-1.) and pi_over_2 = asin 1. and pi_over_4 = (atan2 1. 1.)



(*
(* Somme de chaque composante *)
let rec sum a b = function  
  |[] -> (a, b)
  |(e1,e2)::t ->  sum (a +. float(e1)) (b +. float(e2)) t

(* Moyenne de chaque composante *)
let average l =     
  let (a, b) = sum 0. 0. l in
    (a /. float(List.length(l)), b /. float(List.length(l)))



(* Numérateur de la pente de la droite *)
let rec numOfSlope xaverage yaverage = function
  |[] -> 0.
  |(x,y)::t ->
  (float(x) -. xaverage) *. (float(y) -. yaverage) +. numOfSlope xaverage yaverage t

(* Dénominateur de la pente de la droite *)
let rec denumOfSlope xaverage = function
  |[] -> 0.
  |(x,y)::t ->  (float(x) -. xaverage)*.(float(x) -. xaverage) +. denumOfSlope xaverage t



(* calcule la pente du modele lineaire simple y = a + bx *)
let slope l =
  let (xaverage, yaverage) = average l in
    let n = (numOfSlope xaverage yaverage l) and d = (denumOfSlope xaverage l) in
      if d = 0. then 0. else n /. d



let rec listClean l (min,max) = match l with
  |[] -> []
  |(x,_)::t when (x == min) -> listClean t (min,max)
  |(x,_)::t when (x == max) -> listClean t (min,max)
  |h::t -> h :: listClean t (min,max)


let giveMinMax = function
| [] -> failwith "Liste vide"
| (x, _)::t -> 
  let rec getmM min max = function
    |[] -> (min,max)
    |(x,_)::t when x < min -> getmM x max t
    |(x,_)::t when x > max -> getmM min x t
    |h::t -> getmM min max t
    in
      getmM x x t

let nbPoints = 200

let add_min a l = 
  let (x1, y1) = a.(0) in
  let min = ref x1 and y = ref y1 in
    for i = 1 to 4 do
      let (x2, y2) = a.(i) in
        if x2 != 0 && x2 < !min then
        (
          min := x2;
          y := y2;
        )
      done;
      if !min = 0 then l else (!min, !y) :: l
    

let get_points m = 
let l = ref [] and x = ref 0 and  y = ref 0
  and w = Array.length m and h = Array.length m.(0) in
  let dy = max (h / nbPoints) 1 and arr = Array.make 5 (0, 0) in
    while !y < h - 5 * dy do
      for i = 0 to 4 do
        x := 0;
        arr.(i) <- (0, 0);
        while !x < w/4 do
          if (m.(!x).(!y)) then
          begin
            arr.(i) <- (!x, !y);
            x := w;
          end;
          x := !x + 1;
        done;
      y := !y + dy;
      done;
      l := add_min arr !l;
    done;
    l


(* detecte l'angle de rotation *)
let angleDetection m =
  let m = (Utile.toBool m) in
  let l = get_points m in
  let a = ref (slope !l) in
  let a1 = !a in
    let l2 = ref (listClean !l (giveMinMax !l)) in
      let a2 = ref (slope !l2) in
        while !a /. !a2 > 1.1 || !a /. !a2 < 0.9 do
          l := !l2;
          a := !a2;
          a2 := slope (listClean !l (giveMinMax !l))
        done;
  if atan a1 < 0.2 & atan a1 > -0.2 then
    atan a1 (* angle initial trouve quasi nul *)
  else
          ( atan (1. /. !a))
        


let hough mat = 
        let pi = acos(-1.) and pi02 = asin(1.) in
        let mat = (Utile.toBool mat) in
        let (w,h) = (Array.length mat,Array.length mat.(0)) in
        let diagonal = int_of_float(sqrt(float_of_int(w*w+ h*h))) in
        let matrice_de_vote = Array.make_matrix diagonal ((int_of_float(pi*.100.))+1) 0 in
        let teta_max = ref 0. in
        let vote_max = ref 0  in
        for y=0 to h-1 do
           for x=0 to w-1 do
              if (mat.(x).(y) == false) then
                begin
                let t = ref(-.pi02) in           
                 while ( !t <= pi02 ) do
                  let droite =int_of_float((float x *. (cos !t))+.(float
                  y *. (sin !t))) in
                       if droite>=0 then 
                         begin
                          let teta_i = int_of_float(!t*.100. +. pi02*.100.)in
                           matrice_de_vote.(droite).(teta_i) <-
                                  matrice_de_vote.(droite).(teta_i)+1;
                           if !vote_max < matrice_de_vote.(droite).(teta_i) then
                                  begin
                                  vote_max := matrice_de_vote.(droite).(teta_i);
                                  teta_max := !t;
                                  end;
                         end;
                         t := !t +. 0.01;
                 done;
                end;
           done;
        done;
        (!teta_max +. (pi02 /. 2.))

*)
let get_rotated_coord x y x0 y0 cosa sina = 
  let a = ((float x) -. x0) *. cosa -. ((float y) -. y0) *. sina +. x0
  and b = ((float x) -. x0) *. sina +. ((float y) -. y0) *. cosa +. y0 in
    (a, b)

(* Met dans le pixel x y de dest la valeur calculée
à partir de l'interpolation bilinéaire *)
let get_linear_interpolation src dest w h x y x0 y0 cosa sina =
  let (x1, y1) = get_rotated_coord x y x0 y0 cosa sina in
    let a = int_of_float(floor x1) and b = int_of_float(floor y1) in
      if a >= 0 && b >= 0 && a < w-1 && b < h-1 then
      (
        let dx = x1 -. float a and dy = y1 -. float b in
          let top = (1. -. dx) *. src.(a).(b) +. dx *. src.(a).(b) and
              bot = (1. -. dx) *. src.(a).(b+1) +. dx *. src.(a+1).(b+1) in
            let v = (1. -. dy) *. top +. dy *. bot in
              if v > 0.5 then dest.(x).(y) <- true;
       )



(* Transforme un matrice de booléen en matrice de float *)
let floatMat t =
  let w = Array.length t and h = Array.length t.(0) in
    let m = Array.make_matrix w h 0. in
      for i = 0 to w-1 do
        for j = 0 to h-1 do
          if t.(i).(j) then
            m.(i).(j) <- 1.;
        done;
      done;
      m



(* Fais tourner une image de l'angle a *)
let rotation img a =
  let img = (Utile.toBool img) in
  let w = Array.length img and h = Array.length img.(0)
  and src = floatMat img in
    let x0 = float (w / 2) and y0 = float (h / 2)
    and cosa = cos a and sina = -. sin a
    and dest = Array.make_matrix w h false in
      for i = 0 to w-1 do
        for j = 0 to h - 1 do
          get_linear_interpolation src dest w h i j x0 y0 cosa sina;
        done;
      done;
      (Utile.toImg dest)







let img2matrice img =
    let(w,h) = Utile.get_dims img in
    let matrice = Array.make_matrix w h (255,255,255) in
    for y=0 to h-1 do
        for x=0 to w-1 do
            matrice.(x).(y) <- Sdlvideo.get_pixel_color img x y
        done;
    done;
        (matrice)


        (* Créé une matrice blanche *)
let creat_white_mat x y = 
    let matrix = Array.make_matrix x y (255,255,255) in
    (matrix)



let reduce oldMat =
  let (w,h) = (Array.length oldMat,Array.length oldMat.(0)) in
    let newMat = creat_white_mat(w/2)(h/2) in
  for y = 0 to h/2 -1 do
    for x = 0 to w/2 -1 do
            if(oldMat.(x).(y) =(0,0,0)) then
            begin
              newMat.(x).(y) <- oldMat.(x).(y)
            end    
          done;
  done;
  (newMat)
  


(* matrice est une image transformée en tableau de pixel *)
let hough2 matrice =
  let matrice = reduce (img2matrice (matrice)) in
    (* Constantes *)
    let (w,h) = (Array.length matrice,Array.length matrice.(0)) in
    let diagonal = int_of_float(sqrt(float_of_int(w*w+ h*h))) in
    let matrice_de_vote = Array.make_matrix diagonal ((int_of_float(pi*.100.))+1) 0 in
    (* Variables *)
let teta_max = ref 0. in
let vote_max = ref 0  in
(* Parcours de l'image *)
for y=0 to h-1 do
    for x=0 to w-1 do
        if (matrice.(x).(y) = (0,0,0)) then
            begin
                (* Test l'angle qu'on fera varier de -pi/2 a pi/2 *)
                let t = ref(-.pi_over_2) in
                (* Parcours de l'intervalle d'angle  *)                   
                while ( !t <= pi_over_2 ) do
                    let droite =int_of_float(((float y) *. (cos !t))+.((float
                    x) *. (sin !t))) in
                    if droite >= 0 then (* Remplissement du tableau de vote *)
                        begin
                            let teta_i = int_of_float(!t*.100. +. pi_over_2*.100.)in
                            matrice_de_vote.(droite).(teta_i) <-
                                matrice_de_vote.(droite).(teta_i)+1;
                                if !vote_max < matrice_de_vote.(droite).(teta_i) then
                                    begin
                                        vote_max := matrice_de_vote.(droite).(teta_i);
                                        teta_max := !t;
                                    end;
                                    end;
                                    t := !t +. 0.01; (* Incrémentation de l'angle de 0.01 *) 
                done;
                        end;
                done;
    done;
    print_float(!teta_max);
    (!teta_max)
















(* Détecte et fais tourner l'image *)
let rotate1 m =
  let a = hough2 m in
    rotation m a
(*
let rotate2 m =
  let a = angleDetection m in
    rotation m a

*)
