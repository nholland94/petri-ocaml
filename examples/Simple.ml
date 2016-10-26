open Petri

let arcs = [
  Net.PlaceToTransition (0, 0, 1);
  Net.TransitionToPlace (0, 0, 1);
  Net.TransitionToPlace (2, 0, 1);
  Net.TransitionToPlace (0, 2, 1);
  Net.PlaceToTransition (2, 2, 1);
  Net.PlaceToTransition (1, 1, 1);
  Net.TransitionToPlace (1, 2, 2)
]

let _ =
  let net = Net.make 3 3 arcs in
  Visualize.display_net_graph net
