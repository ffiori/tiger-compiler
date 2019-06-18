structure tigerregalloc :> tigerregalloc =
struct

open Splaymap
open Splayset

type allocation = (tigertemp.temp, tigerframe.register) tigertab.Tabla

(**************** Graph *****************)
fun stringPairCompare ((s1,s2),(t1,t2)) =
    if s1=t1 then String.compare(s2,t2) else String.compare(s1,t1)

val adjSet : ((tigertemp.temp * tigertemp.temp) Splayset.set) ref = 
    ref (empty(stringPairCompare))

val adjList : ((tigertemp.temp, (tigertemp.temp set) ref) Splaymap.dict) ref =
    ref (mkDict(String.compare))

val degree : ((tigertemp.temp, int ref) dict) ref = ref (mkDict(String.compare))
(************** End Graph ***************)

val precolored : ((tigertemp.temp) set) ref = ref (empty(String.compare)) (* TODO where does this get filled? *)
val initial : ((tigertemp.temp) set) ref = ref (empty(String.compare)) (* TODO idem precolored *)

val simplifyWorklist : ((tigertemp.temp) set) ref = ref (empty(String.compare))
val freezeWorklist : ((tigertemp.temp) set) ref = ref (empty(String.compare)) (* nodos de grado bajo, relacionados con moves. (candidatos a coalescer) *)
val spillWorklist : ((tigertemp.temp) set) ref = ref (empty(String.compare)) (* nodos de grado alto *)

val spilledNodes : ((tigertemp.temp) set) ref = ref (empty(String.compare))
val coalescedNodes : ((tigertemp.temp) set) ref = ref (empty(String.compare))
val coloredNodes : ((tigertemp.temp) set) ref = ref (empty(String.compare))

val selectStack : tigertemp.temp list ref = ref [] (* Temporaries removed from the graph. *)

val coalescedMoves : (tigerassem.instr set) ref = ref (empty(tigerassem.compare))
val constrainedMoves : (tigerassem.instr set) ref = ref (empty(tigerassem.compare)) (* moves con src/dst que interfieren. *)
val frozenMoves : (tigerassem.instr set) ref = ref (empty(tigerassem.compare)) (* moves que no van a coalescer. *)
val worklistMoves : (tigerassem.instr set) ref = ref (empty(tigerassem.compare)) (* moves que pueden coalescer. *)
val activeMoves : (tigerassem.instr set) ref = ref (empty(tigerassem.compare)) (* moves que no estan listos para coalescer. (en un principio todos los moves estan aca) *)

val moveList : ((tigertemp.temp, (tigerassem.instr set) ref) dict) ref =
    ref (mkDict(String.compare)) (* moves asociados a un nodo. moveList[u] da los moves que usan a u. *)

fun addEdge (u,v) =
    if u<>v andalso not (member(!adjSet,(u,v)))
    then
        let
            val _ = adjSet := add(!adjSet,(u,v))
            val _ = adjSet := add(!adjSet,(v,u))
            fun f x y =
                if not (member(!precolored,x))
                then 
                    let
                        val adj_list = Splaymap.find(!adjList, x)
                        val _ = adj_list := add(!adj_list, y)
                        val degree_node = Splaymap.find(!degree, x)
                        val _ = degree_node := !degree_node + 1
                    in () end
                else ()
        in (f u v; f v u) end
    else ()

fun adjacent n = 
    difference(!(Splaymap.find(!adjList,n)), addList(!coalescedNodes, !selectStack))

fun nodeMoves n =
    intersection(!(Splaymap.find(!moveList,n)), union(!activeMoves, !worklistMoves))

fun moveRelated n = Splayset.numItems(nodeMoves(n)) <> 0

fun makeWorklist() =
    Splayset.app
    (fn n => 
        if !(Splaymap.find(!degree,n)) >= tigerframe.usable_registers
        then spillWorklist := add(!spillWorklist,n)
        else
            if moveRelated(n)
            then freezeWorklist := add(!freezeWorklist,n)
            else simplifyWorklist := add(!simplifyWorklist,n)
    )
    (!initial);
    val _ = initial := empty(String.compare)

fun first_element set = hd(Splayset.listItems(set))

fun push(n,list) = (n::list)
fun pop (x::xs) = x
  | pop [] = raise Fail "[pop] Stack vacío!\n"

fun enableMoves nodes =
    let fun processMove m =
            if member(!activeMoves,m)
            then 
                let val _ = activeMoves := delete(!activeMoves, m)
                    val _ = worklistMoves := add(!worklistMoves, m)
                in () end
            else ()
    in  
        Splayset.app
        (fn n => Splayset.app processMove (nodeMoves(n)))
        nodes
    end
    
fun decrementDegree node =
    let val deg = Splaymap.find(!degree,node)
        val _ = deg := !deg-1
        val _ = 
            if !deg = tigerframe.usable_registers - 1
            then
                let val _ = enableMoves(Splayset.add(adjacent(node), node))
                    val _ = spillWorklist := delete(!spillWorklist, node)
                in 
                    if moveRelated(node)
                    then freezeWorklist := Splayset.add(!freezeWorklist,node)
                    else simplifyWorklist := Splayset.add(!simplifyWorklist,node)
                end
            else ()
    in () end

fun simplify() =
    let val n = first_element(!simplifyWorklist)
        val _ = simplifyWorklist := delete(!simplifyWorklist, n)
        val _ = push(n, !selectStack)
        val _ = Splayset.app decrementDegree (adjacent(n))
    in () end

fun alloc (frm : tigerframe.frame) (body : tigerassem.instr list) = 
    let
        val (flow_graph, node_list) = tigerflow.instrs2graph body
        val (interf_graph, liveout) : tigerliveness.igraph * (tigergraph.node -> tigertemp.temp list) =
            tigerliveness.interferenceGraph flow_graph
        
        (* Declare and initialize stuff TODO *)
        val _ = simplifyWorklist := empty(String.compare)
        val _ = freezeWorklist := empty(String.compare)
        val _ = spillWorklist := empty(String.compare)
        
        val _ = spilledNodes := empty(String.compare)
        val _ = coalescedNodes := empty(String.compare)
        val _ = coloredNodes := empty(String.compare)

        val _ = selectStack := []
        
        val _ = coalescedMoves := empty(tigerassem.compare)
        val _ = constrainedMoves := empty(tigerassem.compare)
        val _ = frozenMoves := empty(tigerassem.compare)
        val _ = worklistMoves := empty(tigerassem.compare)
        val _ = activeMoves := empty(tigerassem.compare)
        
        val _ = moveList := mkDict(String.compare)
        
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
                    if tigertab.tabEsta(node,ismove) andalso tigertab.tabSaca(node,ismove) (* para que no explote, se puede emprolijar *)
                    then
                        let
                            val _ = live := difference(!live, !use_set)
                            val _ = Splayset.app
                                    (fn temp => 
                                        let val moves_set = Splaymap.find(!moveList, temp) 
                                        in moves_set := Splayset.add(!moves_set, instr) end)
                                    (union(!def_set, !use_set))
                        in worklistMoves := add(!worklistMoves,instr) end
                    else ()
                
                val _ = live := union(!live,!def_set)
                val _ = Splayset.app
                        (fn d => Splayset.app (fn l => addEdge(l,d)) (!live))
                        (!def_set)
                val _ = live := union(!use_set, difference(!live,!def_set))
            in () end
        
        val _ = List.app processInstruction ((List.rev o ListPair.zip) (body, node_list))
        (************************ build() end *************************)
        
        val _ = makeWorklist()
        
        fun loop() =
            let
                val _ =
                    if Splayset.numItems(!simplifyWorklist) <> 0
                    then simplify()
                    else if Splayset.numItems(!worklistMoves) <> 0
                    then () (*TODO*)
                    else if Splayset.numItems(!freezeWorklist) <> 0
                    then () (*TODO*)
                    else if Splayset.numItems(!spillWorklist) <> 0
                    then () (*TODO*)
                    else ()
            in
                if  Splayset.numItems(!simplifyWorklist) = 0 andalso
                    Splayset.numItems(!worklistMoves) = 0 andalso
                    Splayset.numItems(!freezeWorklist) = 0 andalso
                    Splayset.numItems(!spillWorklist) = 0
                then ()
                else loop()
            end
        
(*
        val _ = assignColors()
        val _ =
            if Splayset.numItems(!spilledNodes) <> 0
            then rewriteProgram(!spilledNodes); alloc frm body
            else ()
*)
    in
        (body, tigertab.tabNueva()) (* TODO *)
    end

end
