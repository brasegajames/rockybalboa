val sum : float -> float -> (int * int) list -> float * float
val average : (int * int) list -> float * float
val numOfSlope : float -> float -> (int * int) list -> float
val denumOfSlope : float -> (int * 'a) list -> float
val slope : (int * int) list -> float
val get_rotated_coord :
  int -> int -> float -> float -> float -> float -> float * float
val get_linear_interpolation :
  float array array ->
  bool array array ->
  int -> int -> int -> int -> float -> float -> float -> float -> unit
val floatMat : bool array array -> float array array
val boolMat : int array array -> bool array array
val rotation : bool array array -> float -> bool array array
