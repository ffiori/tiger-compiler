signature tigercolor =
sig
    type allocation = (tigertemp.temp, tigerframe.register) tigertab.Tabla
    
    val color :
        {interference_graph: tigerliveness.igraph,
        initial: allocation,
        spillCost: tigergraph.node -> int,
        registers: tigerframe.register list}
        -> allocation * tigertemp.temp list (* para qué retornar la lista de temps? *)
end
