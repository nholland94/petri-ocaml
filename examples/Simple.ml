open Petri
open Petri.ArcDescriptor

let arcs = [
  Place 0,      Transition 0, 1;
  Transition 0, Place 0,      1;
  Transition 2, Place 0,      1;
  Transition 0, Place 2,      1;
  Place 2,      Transition 2, 1;
  Place 1,      Transition 1, 1;
  Transition 1, Place 2,      2
]

let _ =
  let net = Net.make 3 3 arcs in
  Visualize.display_net_graph net
