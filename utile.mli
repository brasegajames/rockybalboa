val get_dims : Sdlvideo.surface -> int * int
val level : int * int * int -> float
val split : 'a list -> 'a list * 'a list
val merge : 'a list -> 'a list -> 'a list
val merge_sort : 'a list -> 'a list
val show : Sdlvideo.surface -> Sdlvideo.surface -> unit
val display : Sdlvideo.surface -> unit
val createBoolFromImg : Sdlvideo.surface -> bool array array
val createImgFromBool : bool array array -> Sdlvideo.surface -> unit
val drawRect : 'a array array -> int * int * int * int -> 'a -> unit
