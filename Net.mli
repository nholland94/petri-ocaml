type t
type marking = int array
type marked_net = t * marking

(* from id, to id, weight *)
type arc_descriptor =
    PlaceToTransition of int * int * int
  | TransitionToPlace of int * int * int

val make : int -> int -> arc_descriptor list -> t
val make_empty : int -> int -> t
val places : t -> int
val transitions : t -> int
val backward_arcs : t -> int array array
val forward_arcs : t -> int array array
val add_backward_arc : int -> int -> int -> t -> t
val add_forward_arc : int -> int -> int -> t -> t
val add_arc : t -> arc_descriptor -> t
val fire : marked_net -> bool array -> marking
