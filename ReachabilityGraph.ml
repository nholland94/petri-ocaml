open Batteries

let from_net net initial_marking =
  let transition_count = Net.transitions net in
  let backward_arcs = Net.backward_arcs net in
  let input_transitions = Net.inputs net in


  let transitions_to_toggle marking =
    let firable_places = Util.array_collect_indices ((<) 0) marking in
    let collect_connected_transitions place = Util.array_collect_indices ((<) 0) backward_arcs.(place) in
    let output_transitions = List.flatten @@ List.map collect_connected_transitions firable_places in
    List.unique (input_transitions @ output_transitions)
  in

  let singleton_transition_flags transition =
    let flags = Array.make transition_count false in
    flags.(transition) <- true;
    flags
  in

  let reachable_markings = [||] in

  let marking_is_new marking = not (Array.mem marking reachable_markings) in

  let get_marking_children marking =
    let transitions_to_fire = transitions_to_toggle marking in
    let transition_flags_set = List.map singleton_transition_flags transitions_to_fire in
    let marking_postsets = List.unique @@ List.map (Net.fire (net, marking)) transition_flags_set in
    let new_reachable_markings = List.filter marking_is_new marking_postsets in
    
  in
  ()
