(*
	Frames para el 80386 (sin displays ni registers).

		|    argn    |	fp+4*(n+1)
		|    ...     |
		|    arg2    |	fp+16
		|    arg1    |	fp+12
		|	fp level |  fp+8
		|  retorno   |	fp+4
		|   fp ant   |	fp
		--------------	fp
		|   local1   |	fp-4
		|   local2   |	fp-8
		|    ...     |
		|   localn   |	fp-4*n
*)

structure tigerframe :> tigerframe = struct

open tigertree

type level = int

(* Ver pág 260 ! Explica un toque todo lo que se define acá. *)

val fp = "FP"				(* frame pointer *)
val sp = "SP"				(* stack pointer *)
val rv = "RV"				(* return value  *)
val ov = "OV"				(* overflow value (edx en el 386) *)
val wSz = 4					(* word size in bytes *)(* TODO: adjust to the chosen architecture *)
val log2WSz = 2				(* base two logarithm of word size in bytes *)
val fpPrev = 0				(* offset (bytes) *)
val fpPrevLev = 8			(* offset (bytes) *)
val argsInicial = 0			(* words *)
val argsOffInicial = 0		(* words *)
val argsGap = wSz			(* bytes *)
val regInicial = 1			(* reg *)
val localsInicial = 0		(* words *)
val localsGap = ~4 			(* bytes *)
val calldefs = [rv]
val specialregs = [rv, fp, sp]
val argregs = [] (* Registros para paso de argumentos. Depende de la arquitectura. *)
val callersaves = []
val calleesaves = []

type frame = {
    name: string,
    formals: bool list, (* si el argumento escapa o no *)
    locals: bool list,
    actualArg: int ref, (* último argumento generado *)      
    actualLocal: int ref, (* último local generado *)
    actualReg: int ref
}
type register = string
datatype access = InFrame of int (* Offset respecto del frame pointer *) 
                | InReg of tigertemp.label
datatype frag = PROC of {body: tigertree.stm, frame: frame}
              | STRING of tigertemp.label * string
fun newFrame{name, formals} = {
    name=name,
    formals=formals,
    locals=[],
    actualArg=ref argsInicial,
    actualLocal=ref localsInicial,
    actualReg=ref regInicial
}
fun name(f: frame) = #name f
fun string(l, s) = l^tigertemp.makeString(s)^"\n"
(* Devuelve una access list con InFrame(offset) con las localizaciones de donde van a estar los parametros. Pag135. *)
fun formals({formals=f, ...}: frame) = (* : access list *)
    let	fun aux(n, []) = []
        | aux(n, h::t) = InFrame(n)::aux(n+argsGap, t)
    in aux(argsInicial, f) end
fun maxRegFrame(f: frame) = !(#actualReg f)
fun allocArg (f: frame) b = 
	case b of
	true =>
		let	val ret = (!(#actualArg f)+argsOffInicial)*wSz
			val _ = #actualArg f := !(#actualArg f)+1
		in	InFrame ret end
	| false => InReg(tigertemp.newtemp())
fun allocLocal (f: frame) b = 
    case b of
    true =>
        let	val ret = InFrame(!(#actualLocal f)+localsGap)
        in	#actualLocal f:=(!(#actualLocal f)-1); ret end
    | false => InReg(tigertemp.newtemp())
fun exp(InFrame k) e = MEM(BINOP(PLUS, TEMP(fp), CONST k))
    | exp(InReg l) e = TEMP l
fun externalCall(s, l) = CALL(NAME s, l)

(* Se define al final de todo esta función TODO *)
fun procEntryExit1 (frame,body) = body
end
