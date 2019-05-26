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

val fp = "FP"				(* frame pointer *)
val sp = "SP"				(* stack pointer *)
val rv = "RV"				(* return value  *)
val ov = "OV"				(* overflow value (edx en el 386) *)
val wSz = 4					(* word size in bytes *)
val log2WSz = 2				(* base two logarithm of word size in bytes *)
val fpPrev = 0				(* offset (bytes) *)
val fpPrevLev = 0			(* offset (bytes) *) (* Esto era 8 *)
val argsInicial = 3			(* words *)
val argsOffInicial = 1		(* words *)
val argsGap = wSz			(* bytes *)
val regInicial = 1			(* reg *)
val localsInicial = 0		(* words *)
val localsGap = ~4 			(* bytes *)
val calldefs = [rv]
val specialregs = [rv, fp, sp]
val argregs = []
val callersaves = []
val calleesaves = []

datatype access = InFrame of int | InReg of tigertemp.label

type frame = {
	name: string,
	formals: bool list,
	locals: bool list,
	actualArg: int ref,
	actualLocal: int ref,
	actualReg: int ref,
    ftAccesos : access list ref
}
type register = string

datatype frag = PROC of {body: tigertree.stm, frame: frame}
	| STRING of tigertemp.label * string
fun newFrame{name, formals} = {
	name=name,
	formals=formals,
	locals=[],
	actualArg=ref argsInicial,
	actualLocal=ref localsInicial,
	actualReg=ref regInicial,
    ftAccesos = ref [InFrame(0)]
}
fun name(f: frame) = #name f
fun string(l, s) = l^tigertemp.makeString(s)^"\n"
fun formals({formals=f,ftAccesos=fta,...}: frame) = !fta

fun maxRegFrame(f: frame) = !(#actualReg f)
fun allocArg (f: frame) b = 
	case b of
	true =>
		let	val ret = (!(#actualArg f)+argsOffInicial)*wSz
			val _ = #actualArg f := !(#actualArg f)+1
            val _ = (#ftAccesos f) := (!(#ftAccesos f) @ [InFrame ret])
		in	InFrame ret end
	| false =>
        let val tmp = tigertemp.newtemp()
            val _ = (#ftAccesos f) := (!(#ftAccesos f) @ [InReg tmp])
        in InReg tmp end
fun allocLocal (f: frame) b = 
	case b of
	true =>
		let	val ret = InFrame(!(#actualLocal f)+localsGap)
		in	#actualLocal f:=(!(#actualLocal f)-1); ret end
	| false => InReg(tigertemp.newtemp())
fun exp(InFrame k) e = MEM(BINOP(PLUS, TEMP(fp), CONST k))
| exp(InReg l) e = TEMP l
fun externalCall(s, l) = CALL(NAME s, l)

fun procEntryExit1 (frame,body) = body
end
