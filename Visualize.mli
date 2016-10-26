module NetGraph : Graph.Sig.P

val make_net_graph : ?marking:(Net.marking option) -> Net.t -> NetGraph.t
val display_net_graph : ?marking:(Net.marking option) -> Net.t -> unit
