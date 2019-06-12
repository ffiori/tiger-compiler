signature tigerregalloc =
sig
	type allocation = (tigertemp.temp, tigerframe.register) tigertab.Tabla
	
	val alloc : tigerframe.frame -> tigerassem.instr list -> tigerassem.instr list * allocation (* TODO no del todo claro si hay que devolver la tabla de allocation tmb, el libro la devuelve, guido no *)
end
