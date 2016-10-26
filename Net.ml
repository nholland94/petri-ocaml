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

let mat_shape_equal m1 m2 =
  Array.length m1 = Array.length m2 && Array.length m1.(0) = Array.length m2.(0)

let mat_apply fn m1 m2 =
  assert (Array.length m1 > 0);
  assert (mat_shape_equal m1 m2);

  let rows = Array.length m1 in
  let cols = Array.length m1.(0) in
  let mat = Array.make_matrix rows cols m1.(0).(0) in

  let rec loop i j =
    if i < rows then
      if j < cols then begin
        mat.(i).(j) <- fn m1.(i).(j) m2.(i).(j);
        loop i (j + 1)
      end else loop (i + 1) 0
    else mat
  in
  
  loop 0 0

let mat_equal m1 m2 =
  assert (mat_shape_equal m1 m2);

  let rows = Array.length m1 in
  let cols = Array.length m1.(0) in

  let rec loop i j =
    if i < rows then
      if j < cols then begin
        if m1.(i).(j) = m2.(i).(j) then loop i (j + 1) else false
      end else loop (i + 1) 0
    else true
  in
  
  loop 0 0

let valid pn =
  let
    { backward_arcs = ba;
      forward_arcs = fa;
      transition = t} = pn
  in

  let num_places = Array.length ba in
  let num_transitions = if num_places > 0 then Array.length ba.(0) else 0 in

  (num_places > 0 && num_transitions > 0) &&
    List.for_all ((=) num_places) [Array.length fa; Array.length t] &&
    List.for_all ((=) num_transitions) [Array.length fa.(0); Array.length t.(0)] &&
    mat_equal t (mat_apply (-) fa ba)

let fire ({ transition = transition }, marking) enabled_transitions =
  let num_transitions = Array.length transition.(0) in
  assert (Array.length enabled_transitions = num_transitions);

  let toggle_transition switch value = if switch then value else 0 in
  let current_transition = Array.map (vec_apply toggle_transition enabled_transitions) transition in
  Array.map2 (Array.fold_left (+)) marking current_transition
