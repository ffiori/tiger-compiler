structure tigerregalloc :> tigerregalloc =
struct

open Splayset (* may be better Hashset? We don't need order, do we? *)

type allocation = (tigertemp.temp, tigerframe.register) tigertab.Tabla

fun alloc (frm : tigerframe.frame) (body : tigerassem.instr list) = 
    let
        val (flow_graph, node_list) = tigerflow.instrs2graph body
        val (interf_graph, liveout) : tigerliveness.igraph * (tigergraph.node -> tigertemp.temp list) =
            tigerliveness.interferenceGraph flow_graph
        
        (* Declare sets TODO *)
        val worklistMoves : (tigerassem.instr set) ref = ref (empty(tigerassem.compare))
        
        (**************** build() (as in the book) ********************)
        (* WARNING: no entiendo bien qué hace el libro acá, 
         * hace live=liveOut(b) donde b es todo el body de la función.
         * Interpreto que es el liveOut del último nodo del body. *)
        val last_node = List.last node_list (* node_list está en orden con las instrucciones de body *)
        val live : (tigertemp.temp Splayset.set) ref = ref (empty(String.compare))
        val _ = live := addList (!live, liveout last_node)
        
        fun processInstruction (instr,node) =
            let
                val tigerflow.FGRAPH {def=def,use=use,ismove=ismove,control=control} = interf_graph
                val def_set = ref (empty(String.compare))
                val _ = def_set := addList(!def_set, tigertab.tabSaca(node,def))
                val use_set = ref (empty(String.compare))
                val _ = use_set := addList(!use_set, tigertab.tabSaca(node,use))
                
                val _ = 
                    if tigertab.tabSaca(node,ismove)
                    then
                        let
                            val _ = live := difference(!live, !use_set)
                            (* TODO forall *)
                            (* TODO worklistMove *)
                        in () end
                    else ()
                
                val _ = live := union(!live,!def_set)
                (* TODO forall forall *)
                val _ = live := union(!use_set, difference(!live,!def_set))
            in () end
        
        val _ = List.app processInstruction ((List.rev o ListPair.zip) (body, node_list))
        (************************ build() end *************************)
    in
        (body, tigertab.tabNueva()) (* TODO *)
    end

end
