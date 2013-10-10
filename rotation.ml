(*Rotation et détection d'angle*)

let hough matrice =
    (*Déclaration des constantes*)
    let pi = 3.14159265 and pi2 = 1.57079633 in
    let (w,h) = (Array.length matrice, Array.length matrice.(0)) in
    let diagonal = hypot w h in
    let vote_matrice =

