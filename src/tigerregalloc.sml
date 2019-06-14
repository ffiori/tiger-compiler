structure tigerregalloc :> tigerregalloc =
struct

open Splayset (* may be better Hashset? We don't need order, do we? *)
open Splaymap

type allocation = (tigertemp.temp, tigerframe.register) tigertab.Tabla

(******** Graph *********)
fun stringPairCompare ((s1,s2),(t1,t2)) =
    if s1=t1 then String.compare(s2,t2) else String.compare(s1,t1)

val adjSet : ((tigertemp.temp * tigertemp.temp) Splayset.set) ref = 
    ref (empty(stringPairCompare))

val adjList : ((tigertemp.temp, (tigertemp.temp set) ref) dict) ref =
    ref (mkDict(String.compare))

val degree : ((tigertemp.temp, int ref) dict) ref = ref (mkDict(String.compare))
(****** End Graph *******)

val precolored : ((tigertemp.temp) set) ref = ref (empty(String.compare)) (* TODO where does this get filled? *)

fun addEdge (u,v) adjSet adjList =
    if u<>v andalso not (member(!adjSet,(u,v)))
    then
        let
            val _ = adjSet := add(!adjSet,(u,v))
            val _ = adjSet := add(!adjSet,(v,u))
            fun f x y =
                if not (member(!precolored,x))
                then 
                    let
                        val adj_list = find(!adjList, x)
                        val _ = adj_list := add(!adj_list, y)
                        val degree_node = find(!degree, x)
                        val _ = degree_node := !degree_node + 1
                    in () end
                else ()
        in (f u v; f v u) end
    else ()

fun alloc (frm : tigerframe.frame) (body : tigerassem.instr list) = 
    let
        val (flow_graph, node_list) = tigerflow.instrs2graph body
        val (interf_graph, liveout) : tigerliveness.igraph * (tigergraph.node -> tigertemp.temp list) =
            tigerliveness.interferenceGraph flow_graph
        
        (* Declare and initialize stuff TODO *)
        val worklistMoves : (tigerassem.instr set) ref = ref (empty(tigerassem.compare))
        val moveList : ((tigertemp.temp, (tigerassem.instr set) ref) dict) ref =
            ref (mkDict(String.compare))
        
        val _ = adjSet := empty(stringPairCompare)
        val _ = adjList := mkDict(String.compare)
        
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
                            val _ = Splayset.app
                                    (fn temp => 
                                        let val moves_set = Splaymap.find(!moveList, temp) 
                                        in (moves_set := Splayset.add(!moves_set, instr); ()) end)
                                    (union(!def_set, !use_set))
                            val _ = worklistMoves := add(!worklistMoves,instr)
                        in () end
                    else ()
                
                val _ = live := union(!live,!def_set)
                val _ = Splayset.app
                        (fn d => Splayset.app (fn l => addEdge(l,d) adjSet adjList) (!live))
                        (!def_set)
                val _ = live := union(!use_set, difference(!live,!def_set))
            in () end
        
        val _ = List.app processInstruction ((List.rev o ListPair.zip) (body, node_list))
        (************************ build() end *************************)
    in
        (body, tigertab.tabNueva()) (* TODO *)
    end

end
