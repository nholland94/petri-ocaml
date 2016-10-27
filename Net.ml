open Batteries
open ArcDescriptor

type t =
  { backward_arcs: int array array;
    forward_arcs: int array array;
    transition: int array array }
type marking = int array
type marked_net = t * marking

let places net = Array.length net.transition
let transitions net = Array.length net.transition.(0)

let backward_arcs { backward_arcs = ba; _ } = ba
let forward_arcs { forward_arcs = fa; _ } = fa

let make_empty places transitions =
  let make_base_mat () = Array.make_matrix places transitions 0 in
  { backward_arcs = make_base_mat ();
    forward_arcs = make_base_mat ();
    transition = make_base_mat () }

let check_and_set_arc arc_mat i j w =
  if not (i < Array.length arc_mat && j < Array.length arc_mat.(0)) then
    raise (Invalid_argument "index out of bounds")
  else if arc_mat.(i).(j) <> 0 then
    raise (Invalid_argument "arc already set")
  else
    arc_mat.(i).(j) <- w

let add_backward_arc i j w net =
  check_and_set_arc net.backward_arcs i j w;
  net.transition.(i).(j) <- net.transition.(i).(j) - w

let add_forward_arc i j w net =
  check_and_set_arc net.forward_arcs i j w;
  net.transition.(i).(j) <- net.transition.(i).(j) + w

let add_arc net = function
    Place p, Transition t, w -> add_backward_arc p t w net
  | Transition t, Place p, w -> add_forward_arc p t w net
  | _ ->
      raise (Invalid_argument "arcs must go from places to transitions or from transitions to places")

let make places transitions arcs =
  let net = make_empty places transitions in
  List.iter (add_arc net) arcs;
  net

let transitions_where fn mat =
  let place_count = Array.length mat in
  let transition_count = Array.length mat.(0) in
  let valid_transitions = Array.make transition_count false in
  let id x = x in
  let rec loop p t =
    if p < place_count then
      if t < transition_count then begin
        valid_transitions.(t) <- valid_transitions.(t) || fn mat.(p).(t);
        loop p (t + 1)
      end else loop (p + 1) 0
    else Util.array_collect_indices id valid_transitions
  in
  loop 0 0

let active_transitions_sub m1 m2 =
  Util.list_sub (transitions_where ((<) 0) m1) (transitions_where ((<) 0) m2)

let inputs { backward_arcs = ba; forward_arcs = fa; _ } = active_transitions_sub fa ba
let outputs { backward_arcs = ba; forward_arcs = fa; _ } = active_transitions_sub ba fa

let vec_apply fn v1 v2 =
  let cap = Array.length v1 in
  assert (cap = Array.length v2);
  let vec = Array.make cap v2.(0) in

  let rec loop i =
    if i < cap then begin
      vec.(i) <- fn v1.(i) v2.(i);
      loop (i + 1)
    end else
      vec
  in

  loop 0

let fire ({ transition = transition }, marking) enabled_transitions =
  let num_transitions = Array.length transition.(0) in
  assert (Array.length enabled_transitions = num_transitions);

  let toggle_transition switch value = if switch then value else 0 in
  let current_transition = Array.map (vec_apply toggle_transition enabled_transitions) transition in
  Array.map2 (Array.fold_left (+)) marking current_transition

let string_of_net net =
  let { backward_arcs = ba; forward_arcs = fa; _ } = net in
  let ba_str = Util.string_of_int_mat ba in
  let fa_str = Util.string_of_int_mat fa in
  "Backward Arcs\n=============\n" ^ ba_str ^ "\nForward Arcs\n=============\n" ^ fa_str ^ "\n"
