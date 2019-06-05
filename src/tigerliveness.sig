signature tigerliveness = (*page 225*)
sig
    datatype igraph =
        IGRAPH of 
            {graph: tigergraph.graph, (* Interference graph *)
             tnode: tigertemp.temp -> tigergraph.node,
             gtemp: tigergraph.node -> tigertemp.temp, (* Inverse mapping of tnode *)
             moves: (tigergraph.node * tigergraph.node) list} (* Hint for the register allocator, if MOVE m n is in the list,
                                                               * it would be nice to assign m and n the same register. *)

    (* Returns an interference graph and a table mapping each flowgraph node 
     * to the set of temporaries that are live-out at that node. *)
    val interferenceGraph : 
      tigerflow.flowgraph -> igraph*(tigergraph.node -> tigertemp.temp list)

    (* For debugging *)
    val show : outstream * igraph -> unit
end