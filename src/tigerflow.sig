signature tigerflow =  (* Control flow graph - page 223 *)
sig

    datatype flowgraph = FGRAPH of {control: tigergraph.graph, (* directed graph where each node is an instruction *)
                                    def: (tigertemp.temp list) tigergraph.table, (* table of temporaries defined in each node *)
                                    use: (tigertemp.temp list) tigergraph.table, (* table of temporaries used in each node *)
                                    ismove: bool tigergraph.table} (* tells if each instruction is a MOVE *)

    (* Returns a flow graph and a list of nodes that corresponds exactly to the instructions *)
    (* page 224 *)
    val instrs2graph : tigerassem.instr list -> flowgraph * tigergraph.node list 

end
