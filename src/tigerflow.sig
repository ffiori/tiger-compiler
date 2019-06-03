signature tigerflow =  (* Control flow graph - page 223 *)
sig
    datatype flowgraph = FGRAPH of {control: tigergraph.graph,
				    def: (tigertemp.temp list) tigergraph.table,
				    use: (tigertemp.temp list) tigergraph.table,
				    ismove: bool tigergraph.table}

    val instrs2graph : tigerassem.instr list -> flowgraph * tigergraph.node list (*page 224 *)

end