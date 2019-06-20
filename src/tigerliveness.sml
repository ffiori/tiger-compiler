structure tigerliveness :> tigerliveness =
struct

    open tigergraph
    open tigerflow
    open tigertab
    infix -- U

    type igraph = tigerflow.flowgraph
    
    type liveSet = (tigertemp.temp, unit) tigertab.Tabla
    type liveMap = (tigergraph.node, liveSet) tigertab.Tabla 
    
    val livein : liveMap ref = ref (tigertab.tabNuevaEq(tigergraph.eq))
    val liveout : liveMap ref = ref (tigertab.tabNuevaEq(tigergraph.eq))
    
    fun computeLiveness (tigerflow.FGRAPH flow_graph) =
    (* Recorrer todos los nodos n y llenar livein[n] y liveout[n]. Page 214. *)
        let
            fun computeLivenessForNode(current_node : tigergraph.node) =
                let
(*
                    val _ = print(" *** Itero sobre node "^tigergraph.nodename(current_node)^" ********\n")
*)
                    val def_map : ((node, tigertemp.temp list) tigertab.Tabla) = #def flow_graph
                    val use_map = #use flow_graph

                    val def : (tigertemp.temp list) = 
                        case tigertab.tabBusca(current_node, def_map) of
                            SOME temp_list => temp_list
                          | NONE => raise Fail ("Node "^tigergraph.nodename(current_node)^" doesn't exist in the flow graph.\n")
                    val use : (tigertemp.temp list) = 
                        case tigertab.tabBusca(current_node, use_map) of
                            SOME temp_list => temp_list
                          | NONE => raise Fail ("Node "^tigergraph.nodename(current_node)^" doesn't exist in the flow graph.\n")      
                    
                    (* livein_node = use[n] U (liveout[n] - def[n]) *)     
                    val use_node : liveSet = 
                        List.foldl
                        (fn (temp,live_set) => tabInserta(temp, (), live_set))
                        (tabNueva())
                        use
                    val old_liveout_node = if tabEsta(current_node, !liveout) then tabSaca(current_node, !liveout) else tabNueva()
                    val def_table = tabInserList(tabNueva(), List.map (fn x=>(x,())) def)
                    val livein_node = use_node U (old_liveout_node -- def_table)

(*
                    val _ = print("old_liveut_node_table\n")
                    val _ = tigertab.printTabla(old_liveout_node,fn(a,b) => print(a^"\n"))

                    val _ = print("def_table\n")
                    val _ = tigertab.printTabla(def_table,fn(a,b) => print(a^"\n"))

                    val _ = print("use_node\n")
                    val _ = tigertab.printTabla(use_node,fn(a,b) => print(a^"\n"))
*)

                    (* liveout_node = U (in[s]) forall s in succ[n] *)
                    val liveout_node : liveSet = 
                        List.foldl
                        (fn (n,t) => if tabEsta(n, !livein)
                                     then t U (tabSaca(n, !livein))
                                     else t)
                        (tabNueva())
                        (succ(current_node)) (* succ : node -> node list *)

                    val _ = livein := tabInserta(current_node, livein_node, !livein)
                    val _ = liveout := tabInserta(current_node, liveout_node, !liveout)
(*
                    
                    val _ = print("computeLivenessForNode("^tigergraph.nodename(current_node)^"):\n")
                    val _ = (print("livein_node: "); printTabla(livein_node,(fn (t,_)=>print(t^" "))); print("\n"))
                    val _ = (print("liveout_node:"); printTabla(liveout_node,(fn (t,_)=>print(t^" "))); print("\n"))
*)
                in
                    ()
                end
            
            val livein' = !livein
            val liveout' = !liveout
            
            val _ =
                List.app
                (fn node => computeLivenessForNode(node))
                (tigergraph.nodes(#control flow_graph))
            
            fun compareSets(s1,s2) = tabEquals(s1,s2,(fn (x,y)=>x=y))
            
(*
            val _ = print("livein'\n")
            val _ = tigertab.printTabla(livein',fn(a,b) => (print(tigergraph.nodename(a)^": "); tigertab.printTabla(b,fn(a',b') => print(a'^" "))))
            val _ = print("\nlivein\n")
            val _ = tigertab.printTabla(!livein,fn(a,b) => (print(tigergraph.nodename(a)^": "); tigertab.printTabla(b,fn(a',b') => print(a'^" "))))
            
            val _ = print("\nliveout'\n")
            val _ = tigertab.printTabla(liveout',fn(a,b) => (print(tigergraph.nodename(a)^": "); tigertab.printTabla(b,fn(a',b') => print(a'^" "))))
            val _ = print("\nliveout\n")
            val _ = tigertab.printTabla(!liveout,fn(a,b) => (print(tigergraph.nodename(a)^": "); tigertab.printTabla(b,fn(a',b') => print(a'^" "))))
*)
        in
            if tabEquals(livein', !livein, compareSets) andalso tabEquals(liveout', !liveout, compareSets)
            then ()
            else computeLiveness (tigerflow.FGRAPH flow_graph)
        end
    
    fun interferenceGraph flow_graph = 
        let
            val _ = livein := tigertab.tabNuevaEq(tigergraph.eq)
            val _ = liveout := tigertab.tabNuevaEq(tigergraph.eq)
            val _ = computeLiveness flow_graph

            fun getTempList liveout_now node =
                case tabBusca(node,liveout_now) of
                    SOME tab => List.map (#1) (tabAList tab)
                    | _ => raise Fail "[getTempList] node not found! Maybe while using liveout (return value of interferenceGraph).\n"
            val liveout_answer = (!liveout) (*WARNING liveout puede no copiarse acá? y pisarse cuando se llame de nuevo? *)
        in
            getTempList liveout_answer
        end
        
end

