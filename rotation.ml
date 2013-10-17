(* moyenne *)

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




(* Renvoie les coordonnées d'un point après rotation d'angle a autour de x0, y0 *)
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


(* transforme une matrice d'entier en une matrice de booléens (seuil : 1) *)
let boolMat t =
  let w = Array.length t and h = Array.length t.(0) in
    let m = Array.make_matrix w h false in
      for i = 0 to w-1 do
        for j = 0 to h-1 do
          if t.(i).(j) > 0 then
            m.(i).(j) <- true;
        done;
      done;
      m


(* Fais tourner une image de l'angle a *)
let rotation img a =
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
      dest



