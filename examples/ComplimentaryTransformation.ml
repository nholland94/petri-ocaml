open Petri
open Petri.ArcDescriptor

let arcs = [
  Transition 0, Place 0,      1;
  Place 0,      Transition 1, 2;
  Place 0,      Transition 2, 2;
  Transition 2, Place 1,      1;
  Place 1,      Transition 3, 1
]

let _ = Net.make 2 4 arcs |> Visualize.display_net_graph
