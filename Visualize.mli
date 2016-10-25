module NetGraph : Graph.Sig.P

val make_net_graph : Net.t -> NetGraph.t
val display_net_graph : Net.t -> unit
