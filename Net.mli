type t
type marking = int array
type marked_net = t * marking

(* creation *)
val make : int -> int -> ArcDescriptor.t list -> t
val make_empty : int -> int -> t

(* static properties *)
val places : t -> int
val transitions : t -> int
val backward_arcs : t -> int array array
val forward_arcs : t -> int array array

(* calculated properties *)
val inputs : t -> int list
val outputs : t -> int list

(* mutation *)
val add_backward_arc : int -> int -> int -> t -> unit
val add_forward_arc : int -> int -> int -> t -> unit
val add_arc : t -> ArcDescriptor.t -> unit

(* operation *)
val fire : marked_net -> bool array -> marking

(* misc *)
val string_of_net : t -> string
