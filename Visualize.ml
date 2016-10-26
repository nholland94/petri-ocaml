let wrap_html_el tag child = "<" ^ tag ^ ">" ^ child ^ "</" ^ tag ^ ">"

type node_type = Place | Transition

module Node = struct
  type t = node_type * int * int option
  let compare = Pervasives.compare
  let hash = Hashtbl.hash
  let equal = (=)
end

module Transition = struct
  type t = int
  let compare = Pervasives.compare
  let equal = (=)
  let default = 0
end

let id_of_node_type = function
    Place      -> "p"
  | Transition -> "t"

let string_of_node (t, i, _) = id_of_node_type t ^ string_of_int i

let html_of_node (t, i, m_opt) =
  let marking_str = match m_opt with
      Some m -> " - " ^ wrap_html_el "b" (string_of_int m)
    | None   -> ""
  in
  wrap_html_el "i" (string_of_node (t, i, m_opt)) ^ marking_str

let shape_of_node (t, _, _) =
  match t with
      Place      -> `Circle
    | Transition -> `Box

module NetGraph = Graph.Persistent.Digraph.ConcreteLabeled(Node)(Transition)

module DotNetGraph = Graph.Graphviz.Dot(struct
  include NetGraph

  let edge_attributes (_, n, _) = [`Label (string_of_int n); `Decorate false]
  let default_edge_attributes _ = [`Color 4771]
  let get_subgraph _ = None
  let vertex_attributes n =
    [`Shape (shape_of_node n); `HtmlLabel (html_of_node n)]
  let vertex_name n = string_of_node n
  let default_vertex_attributes _ = []
  let graph_attributes _ = []
end)

let make_net_graph ?marking:(marking=None) net =
  let places = Net.places net in
  let transitions = Net.transitions net in
  let ba = Net.backward_arcs net in
  let fa = Net.forward_arcs net in

  let iota_fold fn max base =
    let rec loop acc i =
      if i < max then loop (fn base i) (i + 1)
      else acc
    in

    loop base 0
  in

  let marking_opt_for node_type i =
    match (node_type, marking) with
        Place, Some m -> Some m.(i)
      | _             -> None
  in

  let add_node node_type graph i =
    NetGraph.add_vertex graph (node_type, i, marking_opt_for node_type i)
  in

  let add_edges fn mat graph =
    let rec loop i j graph =
      if i < places then
        if j < transitions then
          let weight = mat.(i).(j) in
          let graph =
            if weight != 0 then NetGraph.add_edge_e graph (fn i j weight) else graph
          in
          loop i (j + 1) graph
        else loop (i + 1) 0 graph
      else graph
    in

    loop 0 0 graph
  in

  let make_backward_arc_edge p t w = (Place, p, (marking_opt_for Place p)), w, (Transition, t, None) in
  let make_forward_arc_edge p t w = (Transition, t, None), w, (Place, p, (marking_opt_for Place p)) in

  NetGraph.empty
    |> iota_fold (add_node Place) places
    |> iota_fold (add_node Transition) transitions
    |> add_edges make_backward_arc_edge ba
    |> add_edges make_forward_arc_edge fa

let display_net_graph ?marking:(marking=None) net =
  let file = open_out_bin "tmp.dot" in
  net |> make_net_graph ~marking:marking |> DotNetGraph.output_graph file;
  close_out file;
  let _ = Unix.system "dot -Tpng tmp.dot | display" in
  ()
