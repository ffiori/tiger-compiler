signature tigerframe =
sig

type frame
type register = string
val rv : tigertemp.temp
val ra : tigertemp.temp
val fp : tigertemp.temp
val sp : tigertemp.temp
val zero : tigertemp.temp
datatype access = InFrame of int | InReg of tigertemp.label
val fpPrev : int
val fpPrevLev : int
val newFrame : {name: tigertemp.label, formals: bool list} -> frame
val name : frame -> tigertemp.label
val string : tigertemp.label * string -> string
val formals : frame -> access list
val allocArg : frame -> bool -> access
val allocLocal : frame -> bool -> access
val allocCallArgs : frame -> int -> unit
val maxRegFrame : frame -> int
val wSz : int
val log2WSz : int
val callersaves : tigertemp.temp list
val calleesaves : tigertemp.temp list
val argregs : tigertemp.temp list
val exp : access -> tigertree.exp -> tigertree.exp
val externalCall : string * tigertree.exp list -> tigertree.exp
val procEntryExit1 : frame * tigertree.stm -> tigertree.stm
val procEntryExit2 : frame * tigerassem.instr list -> tigerassem.instr list
val procEntryExit3 : frame * tigerassem.instr list -> {prolog: string, body: tigerassem.instr list, epilog: string}
datatype frag = PROC of {body: tigertree.stm, frame: frame}
	| STRING of tigertemp.label * string
val specialregs : tigertemp.temp list
val usable_registers : int (* Cantidad de registros que se pueden usar para hacer el coloreo *)
val usable_register_list : tigertemp.temp list (* Registros que se pueden usar para hacer el coloreo*)
end
