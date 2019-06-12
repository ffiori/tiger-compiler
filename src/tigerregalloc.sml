structure tigerregalloc :> tigerregalloc =
struct

type allocation = (tigertemp.temp, tigerframe.register) tigertab.Tabla

fun alloc (frm : tigerframe.frame) (body : tigerassem.instr list) = 
    let
        val (flow_graph,_) = tigerflow.instrs2graph body
        val interf_graph = tigerliveness.interferenceGraph flow_graph
        
    in
        (body, tigertab.tabNueva())
    end

end
