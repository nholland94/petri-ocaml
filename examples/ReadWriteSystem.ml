open Petri
open Petri.ArcDescriptor

(*
 * Place 1 = Reading
 * Place 2 = Writing
 *)

let arcs = [
  Place 0,      Transition 0, 1;
  Place 0,      Transition 1, 1;
  Transition 0, Place 1,      1;
  Transition 1, Place 2,      1;
  Place 1,      Transition 2, 1;
  Place 2,      Transition 3, 1;
  Transition 2, Place 3,      1;
  Transition 3, Place 3,      1;
  Transition 2, Place 0,      1;
  Transition 3, Place 0,      1;
  Place 3,      Transition 0, 1;
  Place 3,      Transition 1, 1
]

let _ =
  Net.make 4 4 arcs |> Visualize.display_net_graph
