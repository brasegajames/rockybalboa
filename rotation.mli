val pi : float
val pi_over_2 : float
val pi_over_4 : float
val get_rotated_coord :
  int -> int -> float -> float -> float -> float -> float * float
val get_linear_interpolation :
  float array array ->
  bool array array ->
  int -> int -> int -> int -> float -> float -> float -> float -> unit
val floatMat : bool array array -> float array array
val rotation : Sdlvideo.surface -> float -> Sdlvideo.surface -> unit
val img2matrice : Sdlvideo.surface -> Sdlvideo.color array array
val creat_white_mat : int -> int -> (int * int * int) array array
val reduce : 
  (int * int * int) array array -> (int * int * int) array array
val hough2 : Sdlvideo.surface -> float
val rotate1 : Sdlvideo.surface -> Sdlvideo.surface -> unit
