val is_in_bounds : int -> int -> Sdlvideo.surface -> bool
val median : Sdlvideo.surface -> Sdlvideo.surface -> unit
val annex : int * int * int -> int -> int * int * int
val annex2 : int * int * int -> int * int * int -> int * int * int
val annex3 : int * int * int -> int -> int -> int * int * int
val final : int * int * int -> int * int * int
val apply_mat : string -> int -> int -> Sdlvideo.surface -> int * int * int
val filter : string -> Sdlvideo.surface -> Sdlvideo.surface -> unit
(*val dropthebass : int -> Sdlvideo.surface -> unit*)