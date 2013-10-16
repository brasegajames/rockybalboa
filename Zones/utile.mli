val get_dims : Sdlvideo.surface -> int * int
val level : int * int * int -> float
val split : 'a list -> 'a list * 'a list
val merge : 'a list -> 'a list -> 'a list
val merge_sort : 'a list -> 'a list
val show : Sdlvideo.surface -> Sdlvideo.surface -> unit