signature tigerregalloc =
sig
	val alloc : tigerframe.frame -> tigerassem.instr list -> tigerassem.instr list (* TODO no del todo claro si hay que devolver la tabla de allocation tmb, el libro la devuelve, guido no *)
end
