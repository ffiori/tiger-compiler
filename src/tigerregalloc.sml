structure tigerregalloc :> tigerregalloc =
struct

open Splaymap
open Splayset

type allocation = (tigertemp.temp, tigerframe.register) tigertab.Tabla

(* Tries to delete element from set. Returns same set if element is not in set. *)
fun safeDelete(set,element) = (delete(set,element) handle _ => set)
(*
fun safeDelete(set,element) = if member(set,element) then delete(set,element) else set
*)

(* Tries to find the value for given key in map. If key is not in map,
 * it inserts it in the map and returns default_value. *)
(* TODO does it make sense to insert it even if we don't return the map? doesn't it get lost? *)
fun safeFind(map,key,default_value) = (Splaymap.find(map,key) handle _ => (Splaymap.insert(map,key,default_value);default_value))
(*
fun safeFind(map,key,default_value) =
	case Splaymap.peek(map,key) of
		SOME value => value
		| NONE => (Splaymap.insert(map,key,default_value); default_value)
*)

(************************ Interference Graph *************************)
fun stringPairCompare ((s1,s2),(t1,t2)) =
    if s1=t1 then String.compare(s2,t2) else String.compare(s1,t1)

val adjSet : ((tigertemp.temp * tigertemp.temp) Splayset.set) ref = 
    ref (empty(stringPairCompare))

val adjList : ((tigertemp.temp, tigertemp.temp set) Splaymap.dict) ref =
    ref (mkDict(String.compare))
val adjList_default_value = empty(String.compare)

val degree : ((tigertemp.temp, int) dict) ref = ref (mkDict(String.compare))
val degree_default_value = 0

fun printIgraphList() =
    let
        val _ = print("Interference graph - Adjacency list model\n\n")
        fun printNeighbors(neighbors) =
            app
            (fn n => print(n^" "))
            neighbors
        val _ =
            Splaymap.app
            (fn (node,neighbors) => (print(node^": "); printNeighbors(neighbors); print("\n")))
            (!adjList)
    in print("\n") end
    
fun printIgraphEdges() =
    let
        val _ = print("Interference graph - Adjacency set model (set of edges)\n\n")
        fun printNeighbors(neighbors) =
            app
            (fn n => print(n^" "))
            neighbors
        val _ =
            app
            (fn (n1,n2) => print(n1^" -> "^n2^"\n"))
            (!adjSet)
    in print("\n") end

fun printIgraph() = (printIgraphList(); printIgraphEdges())
(********************** End Interference Graph ***********************)

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

val moveList : ((tigertemp.temp, tigerassem.instr set) dict) ref =
    ref (mkDict(String.compare)) (* moves asociados a un nodo. moveList[u] da los moves que usan a u. *)
val moveList_default_value = empty(tigerassem.compare)

val alias : ((tigertemp.temp, tigertemp.temp) dict) ref = ref (mkDict(String.compare))
(*
val color : ((tigertemp.temp, string) dict) ref = ref (empty(String.compare)) (* mapea temporales a registros posta *)
*)

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
                        val adj_list = safeFind(!adjList, x, adjList_default_value)
                        val _ = adjList := insert(!adjList, x, add(adj_list, y))
                        val degree_node = safeFind(!degree, x, degree_default_value)
                        val _ = degree := insert(!degree, x, degree_node + 1)
                    in () end
                else ()
        in (f u v; f v u) end
    else ()

(* adjacent : tigertemp.temp -> tigertemp.temp set *)
fun adjacent n = 
    difference(safeFind(!adjList, n, adjList_default_value), addList(!coalescedNodes, !selectStack))

fun nodeMoves n =
    intersection(safeFind(!moveList, n, moveList_default_value), union(!activeMoves, !worklistMoves))

fun moveRelated n = Splayset.numItems(nodeMoves(n)) <> 0

fun makeWorklist() =
    Splayset.app
    (fn n => 
        if safeFind(!degree, n, degree_default_value) >= tigerframe.usable_registers
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
                let val _ = activeMoves := safeDelete(!activeMoves, m)
                    val _ = worklistMoves := add(!worklistMoves, m)
                in () end
            else ()
    in  
        Splayset.app
        (fn n => Splayset.app processMove (nodeMoves(n)))
        nodes
    end
    
fun decrementDegree node =
    let val deg = safeFind(!degree,node,degree_default_value)
        val _ = degree := insert(!degree,node,deg-1)
        val _ = 
            if deg = tigerframe.usable_registers
            then
                let val _ = enableMoves(Splayset.add(adjacent(node), node))
                    val _ = spillWorklist := safeDelete(!spillWorklist, node)
                in 
                    if moveRelated(node)
                    then freezeWorklist := Splayset.add(!freezeWorklist,node)
                    else simplifyWorklist := Splayset.add(!simplifyWorklist,node)
                end
            else ()
    in () end

fun simplify() =
    let val n = first_element(!simplifyWorklist)
        val _ = simplifyWorklist := safeDelete(!simplifyWorklist, n)
        val _ = push(n, !selectStack)
        val _ = Splayset.app decrementDegree (adjacent(n))
    in () end

fun getAlias node =
    if member(!coalescedNodes,node) 
    then getAlias (Splaymap.find(!alias,node) handle _ => raise Fail "[getAlias] node not found in alias. Should not happen.\n")
    else node

fun addWorklist u =
    if (not(member(!precolored,u))) andalso (not(moveRelated u)) andalso safeFind(!degree,u,0) < tigerframe.usable_registers
    then (
        freezeWorklist := safeDelete(!freezeWorklist,u);
        simplifyWorklist := add(!simplifyWorklist,u)
    )
    else ()    

fun OK(t,r) =
    safeFind(!degree,t,degree_default_value) < tigerframe.usable_registers orelse
    member(!precolored,t) orelse
    member(!adjSet,(t,r))

fun conservative node_set =
    let val k = 
        foldl
        (fn (node,answer) => 
            if safeFind(!degree,node,0) >= tigerframe.usable_registers
            then answer+1
            else answer)
        0
        node_set
    in k < tigerframe.usable_registers end

fun combine(u,v) = () (*TODO*)

fun coalesce() =
    let
        val m = first_element (!worklistMoves)
        val (x,y) = (* TODO cuál es x y cuál es y? src y dst, o al revés? *)
            case m of
                tigerassem.MOVE {assem=assem, dst=dst, src=src} => (getAlias src, getAlias dst)
                | _ => raise Fail "[coalesce] instruction not MOVE in worklistMoves\n"
        val (u,v) = if member(!precolored,y) then (y,x) else (x,y)
        val _ = safeDelete(!worklistMoves,m)
        
        fun bigCondition() =
            let
                val left = 
                    foldl
                    (fn (node,answer) => OK(node,u) andalso answer)
                    (member(!precolored,u))
                    (adjacent v)
                val right = not(member(!precolored,u)) andalso conservative(union(adjacent u, adjacent v))    
            in 
                left orelse right
            end
    in
        if u=v
        then ((coalescedMoves := add(!coalescedMoves,m)); addWorklist(u))
        else if member(!precolored,u) orelse member(!adjSet,(u,v))
        then (
            constrainedMoves := add(!constrainedMoves,m);
            addWorklist(u);
            addWorklist(v)
        )
        else if bigCondition()
        then (
            coalescedMoves := add(!coalescedMoves,m);
            combine(u,v);
            addWorklist(u)
        )
        else (activeMoves := add(!activeMoves,m))
    end

fun alloc (frm : tigerframe.frame) (body : tigerassem.instr list) = 
    let
        val _ = print("ASSEM LIST: \n ")
        val _ = List.app (fn w => print(tigerassem.format (fn f => f) w)) body
        val (flow_graph, fnode_list) = tigerflow.instrs2graph body
        val tigerflow.FGRAPH {def=def,use=use,ismove=ismove,control=control} = flow_graph
        val _ = print("FLOW GRAPH: \n ")
        val _ = tigerflow.print_graph(control)

        val liveout : tigergraph.node -> tigertemp.temp list =
            tigerliveness.interferenceGraph flow_graph
        
        (* Declare and initialize stuff TODO por ahora estoy dejando todo vacío pero algunas cosas tienen que tener cosas *)
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
        
        val _ = alias := mkDict(String.compare)
        (*
        val color : ((tigertemp.temp, string) dict) ref = ref (empty(String.compare)) (* mapea temporales a registros posta *)
        *)
        
        (**************** build() (as in the book) ********************)
        fun processInstruction (instr,fnode) =
            let
                val live : (tigertemp.temp Splayset.set) ref = ref (empty(String.compare))
                val _ = live := addList (!live, liveout fnode)
                val def_set = ref (empty(String.compare))
                val _ = def_set := addList(!def_set, tigertab.tabSaca(fnode,def))
                val use_set = ref (empty(String.compare))
                val _ = use_set := addList(!use_set, tigertab.tabSaca(fnode,use))
                
                val _ = 
                    if tigertab.tabEsta(fnode,ismove) andalso tigertab.tabSaca(fnode,ismove) (* para que no explote, se puede emprolijar *)
                    then
                        let
                            val _ = live := difference(!live, !use_set)
                            val _ = Splayset.app
                                    (fn temp => 
                                        let val moves_set = safeFind(!moveList, temp, moveList_default_value) 
                                        in moveList := insert(!moveList, temp, add(moves_set, instr)) end)
                                    (union(!def_set, !use_set))
                        in worklistMoves := add(!worklistMoves,instr) end
                    else ()
                
(*
                val _ = live := union(!live,!def_set)
*)
                val _ = Splayset.app
                        (fn d => Splayset.app (fn l => addEdge(l,d)) (!live))
                        (!def_set)
(*
                val _ = live := union(!use_set, difference(!live,!def_set))
*)
            in () end
        
        val _ = List.app processInstruction ((List.rev o ListPair.zip) (body, fnode_list))
        val _ = printIgraph() (* DEBUGGING *)
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
