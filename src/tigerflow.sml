structure tigerflow :> tigerflow =
struct

  open tigergraph
  open tigerassem
  open tigertab

  datatype flowgraph = FGRAPH of {control: tigergraph.graph,
                                  def: (tigertemp.temp list) tigergraph.table,
                                  use: (tigertemp.temp list) tigergraph.table,
                                  ismove: bool tigergraph.table}

  (* instrs2graph : tigerassem.instr list -> flowgraph * tigergraph.node list *)
  (* Takes a list of instructions and returns a flow graph,
    along with a list of nodes that correspond exactly to the instructions. *)

  fun instrs2graph il = 
      let
          (* 0 - Create a new flow graph *)
          val control = newGraph()

          (* 1 - Create the nodes of the graph *)
          fun create_nodes [] = []
           |  create_nodes ((LABEL l)::is) =   (* labels are not proper instructions, so no node is created *)
                let                            (* However, we do save the node of the next instruction, as it
                                                  is where control will go if the program jumps to LABEL l *)            
                  val ns = create_nodes is
                  val p = case ns of
                               []                 => raise Fail "[instrs2graph] label suelta, no deberia pasar"
                            | ((inst,next_node)::_) => (LABEL l,next_node)
                in
                  p::ns
                end
           |  create_nodes (i::is) = (i,newNode(control))::(create_nodes is) (* For each non-Label instruction, a node is created *)

          val instr_node_pairs = create_nodes il

          (* 2 - Store jump references *)
          val ord_labels = List.mapPartial
						   ( fn (LABEL{assem,lab=l}, node) => SOME (l,node)
						       | _ => NONE )
						   instr_node_pairs
          val label_ref_nodes = tabInserList(tabNueva(), ord_labels)  (* For each label, save the node that follows it on a table. *)

          (* 3 - Add edges to the graph *)
          fun create_edges ((OPER {src,dst,jump=SOME js,...}, n) :: ns) =  
            (* If it is a jump node, connect it with its potential destination nodes *)
              let
                val nodes = List.map (fn j => 
                                        case tabBusca(j,label_ref_nodes) of
                                            NONE => raise Fail "[instr2graph] label desconocida"
                                          | SOME node => node
                                      ) js

                val _ = List.app (fn node => mk_edge{from=n,to=node}) nodes (* WARNING: It could make it multiedged, do we support that? Sería en el caso en que más de un JUMP potencial de una instrucción va a la misma etiqueta *)
              in
                create_edges(ns)
              end
            
            (* Nodes that cannot jump are connected with the node of the next instruction *)
            | create_edges ((i1,n1)::(i2,n2)::is) = (mk_edge{from=n1,to=n2} ; create_edges((i2,n2)::is))
            | create_edges _ = () (* one or zero instructions left, no out edges *)

          val _ = create_edges instr_node_pairs

          (* 4 - The graph is ready. Get extra information required for the FGRAPH: *)
          fun is_label (LABEL _) = true
            | is_label _         = false
            
          val nodes_not_labels = List.filter (fn (x,n) => not(is_label x)) instr_node_pairs

          fun get_def (OPER {assem=_,dst=d,src=s,...},n)  = (n,d)
            | get_def (MOVE {assem=_,dst=d,src=s},n)      = (n,[d])
            | get_def _ = raise Fail "[get_def] Should not happen. Is there any LABEL l remaining?"
          val def = tabInserList(tigertab.tabNuevaEq tigergraph.eq, List.map get_def nodes_not_labels)

(*
val _ = tigertab.tabBusca(tigergraph.newNode(tigergraph.newGraph()),def)
*)

          fun get_src (OPER {assem=_,dst=d,src=s,...},n)  = (n,s)
            | get_src (MOVE {assem=_,dst=d,src=s},n)      = (n,[s])
            | get_src _ = raise Fail "[get_src] Should not happen. Is there any LABEL l remaining?"
          val use = tabInserList(tigertab.tabNuevaEq tigergraph.eq, List.map get_src nodes_not_labels)

          fun is_move (MOVE _,n)  = (n,true)
            | is_move (_,n)       = (n,false)
          val ismove = tabInserList(tigertab.tabNuevaEq tigergraph.eq, List.map is_move nodes_not_labels)

      in
          (FGRAPH{control = control,
                  def = def,
                  use = use,
                  ismove = ismove},
          List.map (fn (x,y) => y) nodes_not_labels)
      end

end (* struct *)
