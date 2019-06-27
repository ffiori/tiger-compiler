structure tigerregalloc :> tigerregalloc =
struct

open Splaymap
open Splayset

type allocation = (tigertemp.temp, tigerframe.register) dict

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

val debug : bool ref = ref (false)

fun printIgraphList() =
    let
        val _ = print("\n[Coloreo] INTERFERENCE GRAPH - Adjacency list model\n")
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
        val _ = print("\n[Coloreo] INTERFERENCE GRAPH - Adjacency set model (set of edges)\n")
        fun printNeighbors(neighbors) =
            app
            (fn n => print(n^" "))
            neighbors
        val _ =
            app
            (fn (n1,n2) => print(n1^" -> "^n2^"\n"))
            (!adjSet)
    in print("\n") end

fun printIgraph() = printIgraphList() (*; printIgraphEdges() *)

(********************** End Interference Graph ***********************)
val precolored : ((tigertemp.temp) set) ref = ref (Splayset.addList((Splayset.empty String.compare), tigerframe.usable_register_list) ) (* It should never be changed*)
val initial : ((tigertemp.temp) set) ref = ref (empty(String.compare)) 

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

val color : ((tigertemp.temp, tigerframe.register) dict) ref = ref (mkDict(String.compare)) (* mapea temporales a registros posta *)


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

(* nodemoves : tigertemp.temp -> tigerassem.instr set *)
fun nodeMoves n =
    intersection(safeFind(!moveList, n, moveList_default_value), union(!activeMoves, !worklistMoves))

fun moveRelated n = Splayset.numItems(nodeMoves(n)) <> 0

(*  Para cada nodo en initial, llenar las listas spillWorklist/freezeWorklist/simplifyWorklist *)
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
fun top(ls) = hd(ls)
fun pop(ls) = List.drop(ls,1)

(* Para cada nodo n en nodes, agarra todos los moves involucrados a n que no estaban listos
   para coalescer y los marca como listos para coalescing*)
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
        val _ = if !debug then print("Simplify.. Node "^n^"has been removed\n") else ()
        val _ = simplifyWorklist := safeDelete(!simplifyWorklist, n)
        val _ = selectStack:= push(n, !selectStack)
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

fun combine(u,v) = 
    let 
        val _ = if member(!freezeWorklist,v) 
                then freezeWorklist:=delete(!freezeWorklist,v)
                else spillWorklist :=delete(!spillWorklist,v)
        val _ = coalescedNodes:=add(!coalescedNodes,v)
        val _ = alias := insert(!alias,v,u);
        val mlu = safeFind(!moveList, u, moveList_default_value)
        val mlv = safeFind(!moveList, v, moveList_default_value)
        val _ = moveList := insert(!moveList,u,union(mlu,mlv))  (*Errata*)
        val _ = enableMoves(Splayset.singleton String.compare v)  (*Errata*)
        val _ = Splayset.app (fn t => (addEdge(t,u) ; decrementDegree t) ) (adjacent v)
        val _ = if (safeFind(!degree, u, degree_default_value)>=tigerframe.usable_registers) andalso member(!freezeWorklist,u) 
                then (freezeWorklist := delete(!freezeWorklist,u); spillWorklist := add(!spillWorklist,u) )
                else ()
    
    in
        ()
    end  

fun coalesce() =
    let
        val m = first_element (!worklistMoves)
        val (x,y) =  (*x es src, y es dst*)
            case m of
                tigerassem.MOVE {assem=assem, dst=dst, src=src} => (getAlias src, getAlias dst)
                | _ => raise Fail "[coalesce] instruction not MOVE in worklistMoves\n"
        val (u,v) = if member(!precolored,y) then (y,x) else (x,y)
        val _ = if !debug then print("Coalescing move "^x^" -> "^y^", whose aliases are "^u^" and "^v^"\n") else ()
        val _ = worklistMoves := safeDelete(!worklistMoves,m)
        
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
        else if member(!precolored,v) orelse member(!adjSet,(u,v))
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

fun freezeMoves(u) =
    let fun freeze_single_move m =
            let val (x,y) = case m of
                                tigerassem.MOVE {assem=assem, dst=dst, src=src} => (getAlias src, getAlias dst)
                                | _ => raise Fail "[coalesce] instruction not MOVE in worklistMoves\n"
                val v = if (getAlias y = getAlias u) then getAlias x else getAlias y
                val _ = if !debug then  print("Freezing move "^x^" -> "^y^", whose aliases are "^u^" and "^v^"\n") else ()
                val _ = activeMoves := safeDelete(!activeMoves,m)
                val _ = frozenMoves := add(!frozenMoves,m)
                val _ = if (Splayset.isEmpty(nodeMoves(v)) andalso (safeFind(!degree, v, degree_default_value) < tigerframe.usable_registers))
                        then (freezeWorklist := safeDelete(!freezeWorklist,v); simplifyWorklist := add(!simplifyWorklist,v))
                        else ()
            in
                ()
            end

        val _ = Splayset.app freeze_single_move (nodeMoves(u))
    in 
        ()
    end

fun freeze() =
    let
        val _ = if !debug then  print("Freeze invoked ...") else ()
        val u = first_element (!freezeWorklist)
        val _ = freezeWorklist := safeDelete(!freezeWorklist,u)
        val _ = simplifyWorklist := add(!simplifyWorklist,u)
        val _ = freezeMoves(u)
    in
        ()
    end

fun selectSpill() = 
    let
        fun heuristic ls = first_element (!ls) (* TODO: Algo mas inteligente (O NO) *)
        val m = heuristic spillWorklist
        val _ = if !debug then  print("Spilling node "^m^"\n") else ()
        val _ = spillWorklist := safeDelete(!spillWorklist,m)
        val _ = simplifyWorklist := add(!simplifyWorklist,m)
        val _ = freezeMoves(m)

    in
        ()
    end

fun assignColors() = 
    let
        val _ = if !debug then  print("[Coloreo]ASSIGNING COLORS...\n") else ()
        fun while_body() = 
            if (List.length(!selectStack)>0) 
            then
                let
                    val n = top(!selectStack)
                    val _ = selectStack := pop(!selectStack)
                    val okColors = ref ( addList(empty(String.compare),tigerframe.usable_register_list) )

                    val _ = Splayset.app
                            (fn w => if (member(!precolored,getAlias(w)) orelse member(!coloredNodes,getAlias(w)))
                                    then okColors := safeDelete(!okColors,Splaymap.find(!color, getAlias(w)))
                                    else () )
                            (safeFind(!adjList, n, adjList_default_value))

                    val _ = if (Splayset.isEmpty(!okColors))
                            then (
                                let
                                    val _ = if !debug then print(":( node "^n^" turned out to be an actual spill  \n") else ()
                                    val _ = spilledNodes := add(!spilledNodes,n)
                                in
                                    ()
                                end)
                            else (
                                let
                                    val _ = coloredNodes := add(!coloredNodes,n)
                                    val c = first_element(!okColors)
                                    val _ = if !debug then  print("node "^n^" successfully assigned color "^c^"...\n") else ()
                                    val _ = color := insert(!color,n,c)
                                in 
                                    ()
                                end
                            )
                in
                    while_body()
                end
            else
                ()

        val _ = while_body() 


        val _ = Splayset.app
                (fn n => if member(!precolored,getAlias(n)) 
                         then color := insert(!color,n,getAlias(n))
                         else color := insert(!color,n,Splaymap.find(!color, getAlias(n))))
                (!coalescedNodes)
        
    in
        ()  
    end

fun rewriteProgram(frm,body) = 
    let
        val _ = if !debug then print("\n[Coloreo] Rewriting program...\n") else ()
        val newTemps = ref(Splayset.empty(String.compare))
        
        (* devuelve lista de instr correspondientes a fetchear y storear el valor que estaba en el temporal node en memoria. *)
        fun processInstr node address instr =
            let
                val ilist_src =
                    if List.exists (fn x=>x=node) (tigerassem.getsrc instr)
                    then 
                        let
                            val t = tigertemp.newtemp()
                            val _ = newTemps := add(!newTemps,t)
                            val offset = case address of  (* Que pasa si offset es un valor muy grande?? *)
                                tigerframe.InFrame k => k
                                | _ => raise Fail "rewriteProgram no deberían haber InReg acá\n"
                            val fetch_instr =
                                tigerassem.OPER{
                                    assem="LD `d0, "^tigerpp.ppint offset^"("^tigerframe.fp^")\n",
                                    dst=[t], 
                                    src=[], 
                                    jump=NONE }
                            val instr' = tigerassem.replaceTempSrc instr node t                         
                        in 
                            [fetch_instr,instr'] 
                        end
                    else [instr]
            
                val instr' = List.last ilist_src
                val ilist_dst =
                    if List.exists (fn x=>x=node) (tigerassem.getdst instr)
                    then 
                        let
                            val t = tigertemp.newtemp()
                            val _ = newTemps := add(!newTemps,t)
                            val offset = case address of 
                                tigerframe.InFrame k => k
                                | _ => raise Fail "rewriteProgram no deberían haber InReg acá\n"
                            val store_instr =
                                tigerassem.OPER{
                                    assem="SD `s0, "^tigerpp.ppint offset^"("^tigerframe.fp^")\n",
                                    dst=[], 
                                    src=[t], 
                                    jump=NONE }
                            val instr'' = tigerassem.replaceTempDst instr' node t

                        in [instr'',store_instr] end
                    else [instr']
            in
                if List.length(ilist_src)>1 then List.take(ilist_src,1)@ilist_dst else ilist_dst
            end
            
        fun replaceTemp(node,body) =
            let
                val address = tigerframe.allocLocal frm true 
                val _ = if !debug then print("Old body has size "^Int.toString(List.length(body))^"\n") else ()
                val body' = List.concat(List.map (processInstr node address) body)
                val _ = if !debug then print("New body has size "^Int.toString(List.length(body'))^"\n") else ()
            in body' end
    
        val new_body =
            List.foldl
            replaceTemp
            body
            (listItems(!spilledNodes))
        
        val _ = spilledNodes := empty(String.compare)
        val _ = initial := union(!coloredNodes, union(!coalescedNodes,!newTemps))
        val _ = if !debug then print("Next initial set is: ") else ()
        val _ = if !debug then  Splayset.app (fn n => print(n^" ")) (!initial) else ()
        val _ = if !debug then  print("\n") else ()
        val _ = coloredNodes := empty(String.compare)
        val _ = coalescedNodes := empty(String.compare)
    in
        new_body
    end

fun allocAux (frm : tigerframe.frame) (body : tigerassem.instr list)  = 
    let

        (* Declare and initialize  *)

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
        val _ = color := mkDict(String.compare) (* mapea temporales a registros posta *)

        val _ =  Splayset.app (fn pn => color := insert(!color,pn,pn)) (!precolored)

        val _ = degree:= mkDict(String.compare)
        (* Calculate flow graph and then do liveness analysis *)

        val _ = if !debug then  (print("\n*******************************************************************************\n");
                                 print("[Coloreo] Welcome to a new iteration \n");
                                 print("[Coloreo] INPUT ASSEM PROGRAM: \n ")) else ()
        val _ = if !debug then  List.app (fn w => print(tigerassem.format (fn f => f) w)) body else ()
        
        val (flow_graph, fnode_list) = tigerflow.instrs2graph body
        val tigerflow.FGRAPH {def=def,use=use,ismove=ismove,control=control} = flow_graph

        
        val _ = if !debug then  print("\n[Coloreo] CALCULATED FLOW GRAPH: \n ") else ()
        val _ = if !debug then tigerflow.print_graph(control) else ()
        val _ = if !debug then  print("move related:") else ()
        val _ = if !debug then  (List.app (fn (n,b) => print(tigergraph.nodename(n)^" ")) (tigertab.tabAList(ismove))) else ()
        val _ = if !debug then  print("\n") else ()

        val liveout : tigergraph.node -> tigertemp.temp list =
            tigerliveness.calculateLiveout flow_graph
        
        (* 
         val _ = print("LIVENESS ANALYSIS: \n ")
        val _ = List.app (fn n=>(print("Node "^tigergraph.nodename(n)^": "); List.app (fn t=>print(t^" ")) (liveout n); print("\n"))) fnode_list
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
                

                val _ = live := union(!live,!def_set)

                val _ = Splayset.app
                        (fn d => Splayset.app (fn l => addEdge(l,d)) (!live))
                        (!def_set)

            in () end

        fun  filterLabel (tigerassem.LABEL _) = NONE
            |filterLabel (inst) = SOME inst

        val _ = List.app processInstruction ((List.rev o ListPair.zip) (List.mapPartial filterLabel body, fnode_list))

        val _ = if !debug then printIgraph() else ()

        (************************ build() end *************************)
        
        val _ = makeWorklist()
        val _ = if !debug then print("\n[Coloreo] STARTING SIMPLIFY-COALESCE-FREEZE-SPILL LOOP... \n\n") else ()

        fun loop() =
            let
                val _ = if !debug then print("Loop iteration... ") else ()
                val _ =
                    if Splayset.numItems(!simplifyWorklist) <> 0
                    then simplify()
                    else if Splayset.numItems(!worklistMoves) <> 0
                    then coalesce() 
                    else if Splayset.numItems(!freezeWorklist) <> 0
                    then freeze() 
                    else if Splayset.numItems(!spillWorklist) <> 0
                    then selectSpill() 
                    else ()
            in
                if  Splayset.numItems(!simplifyWorklist) = 0 andalso
                    Splayset.numItems(!worklistMoves) = 0 andalso
                    Splayset.numItems(!freezeWorklist) = 0 andalso
                    Splayset.numItems(!spillWorklist) = 0
                then ()
                else loop()
            end
        
        val _ = loop()
        val _ = if !debug then print("Loop finished\n\n") else ()
        val _ = assignColors()
        val answer =
            if Splayset.numItems(!spilledNodes) <> 0
            then 
                let val _ = if !debug then  print("[Coloreo] Your program has to be rewritten :( \n\n") else ()
                    val body = rewriteProgram(frm,body)
                    val body = allocAux frm body 
                in
                    body
                end
            else body
    in
        answer
    end


fun alloc (frm : tigerframe.frame) (body : tigerassem.instr list) debug_flag = 
    let 
        val _ = debug := debug_flag
        fun   getAllTemps (tigerassem.LABEL _, s) = s
            | getAllTemps (tigerassem.OPER {src=sc, dst=ds,...}, s) = addList(s,sc@ds)
            | getAllTemps (tigerassem.MOVE {src=sc, dst=ds,...}, s) = addList(s,[sc,ds])

        val allTemps = List.foldl getAllTemps (empty String.compare) body
        val _ = initial := difference(allTemps,!precolored) 
    in
        (allocAux frm body,!color) 
    end
end

