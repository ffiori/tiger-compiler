signature tigerregalloc =
sig
(*
	type allocation = (tigertemp.temp, tigerframe.register) tigertab.Tabla
*)
	type allocation = (tigertemp.temp, tigerframe.register) Splaymap.dict
	
	val alloc : tigerframe.frame -> tigerassem.instr list -> bool -> tigerassem.instr list * allocation
end
