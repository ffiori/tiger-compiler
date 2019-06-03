signature tigergraph =
sig
    type graph
    type node
    
    val nodes: graph -> node list
    val succ: node -> node list
    val pred: node -> node list
    val adj: node -> node list (* adj(n) = succ(n) U pred(n) *)
    val eq: node*node -> bool
    
    val newGraph: unit -> graph  (* Creates an emtpy, directed graph*)
    val newNode : graph -> node  
    exception GraphEdge
    val mk_edge: {from: node, to: node} -> unit 
    val rm_edge: {from: node, to: node} -> unit

    val nodename: node->string  (* for debugging *)

    type 'info table = (node,'info) tigertab.Tabla (* Para guardar informacion de los nodos*)

end