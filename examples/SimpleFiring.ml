open Petri
open Petri.ArcDescriptor

let arcs = [
  Place 0,      Transition 0, 1;
  Place 0,      Transition 1, 1;
  Transition 0, Place 1,      1;
  Transition 1, Place 2,      1
]

let initial_marking = [|2; 0; 0|]

let _ =
  let net = Net.make 3 2 arcs in
  Visualize.display_net_graph ~marking:(Some initial_marking) net;
  let next_marking = Net.fire (net, initial_marking) [|true; true|] in
  Visualize.display_net_graph ~marking:(Some next_marking) net
