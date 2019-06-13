structure tigerregalloc :> tigerregalloc =
struct

type allocation = (tigertemp.temp, tigerframe.register) tigertab.Tabla

fun alloc (frm : tigerframe.frame) (body : tigerassem.instr list) = 
    let
        val (flow_graph, node_list) = tigerflow.instrs2graph body
        val (interf_graph, liveout) : tigerliveness.igraph * (tigergraph.node -> tigertemp.temp list) =
            tigerliveness.interferenceGraph flow_graph
        
        (* build() in the book *)
        (* WARNING: no entiendo bien qué hace el libro acá, hace liveOut(b) done b es todo el body.
         * Interpreto que es el liveOut del último nodo del body. *)
        val last_node = List.last node_list (* node_list está en orden con las instrucciones de body *)
        val live = (liveout last_node) (* TODO pasar a splayset o algo así *)
        fun processInstruction (instr,node) =
            let
                val tigerflow.FGRAPH {def=def,use=use,ismove=ismove,control=control} = interf_graph
                val _ = 
                    if tigertab.tabSaca(node,ismove)
                    then
                        ()
                    else ()
            in
                ()
            end
        val _ = List.app processInstruction ((List.rev o ListPair.zip) (body, node_list))
    in
        (body, tigertab.tabNueva()) (* TODO *)
    end

end
