(*pag 207*)
signature tigercodegen = 
sig

	val codegen : tigerframe.frame -> tigertree.stm -> tigerassem.instr list
	val codestring : (string * string) -> string
end
