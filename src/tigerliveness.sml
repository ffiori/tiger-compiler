structure tigerliveness :> tigerliveness =
struct
(*
    datatype igraph =
        IGRAPH of 
            {graph: tigergraph.graph, (* Interference graph *)
             tnode: tigertemp.temp -> tigergraph.node,
             gtemp: tigergraph.node -> tigertemp.temp, (* Inverse mapping of tnode *)
             moves: (tigergraph.node * tigergraph.node) list} (* Hint for the register allocator, if MOVE m n is in the list,
                                                               * it would be nice to assign m and n the same register. *)
*)
    type igraph = tigerflow.flowgraph
    
    type liveSet = (tigertemp.temp, unit) tigertab.Tabla
    type liveMap = (tigergraph.node, liveSet) tigertab.Tabla 
    
    val livein = tigertab.tabNueva()
    val liveout = tigertab.tabNueva()
(*
    val global_live_map : liveMap = tigertab.tabNueva()
*)
(*
  datatype flowgraph = FGRAPH of {control: tigergraph.graph,
                                  def: (tigertemp.temp list) tigergraph.table,
                                  use: (tigertemp.temp list) tigergraph.table,
                                  ismove: bool tigergraph.table}
*)
    
    fun build_live_map (tigerflow.FGRAPH flow_graph) =
(*
        recorrer todos los nodos n y llenar livein[n] y liveout[n]
        page 214
*)
        let
            fun computeLiveness(current_livein, current_liveout, node) =
                let
                    val def_map = #def flow_graph
                    val use_map = #use flow_graph
(*
                    val def =
                        case tigertab.tabBusca(node,def_map) of
                            SOME temp_list => temp_list
                          | NONE => raise Fail ("Node "^tigergraph.nodename(node)^" doesn't exist in the flow graph.\n")
*)
                    val current_livein' = current_livein
                    val current_liveout' = current_liveout
                    
                in
                    ()
                end
        in
            List.app
            (fn n => computeLiveness(tigertab.tabNueva(), tigertab.tabNueva(), n))
            (tigergraph.nodes(#control flow_graph))
        end
    
    fun interferenceGraph flow_graph = 
        let
            val _ = build_live_map flow_graph
(*
        at each flow node n add edges (di,tj) forall di in def(n) and forall ti in global_live_map(n)
*)
        in
            (flow_graph,(fn x=>[]))
        end
        
end

