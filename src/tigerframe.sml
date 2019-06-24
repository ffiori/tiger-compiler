(*
    Frames para el 80386 (sin displays ni registers).

        |    argn    |  fp+4*(n+1)
        |    ...     |
        |    arg2    |  fp+16
        |    arg1    |  fp+12
        |   fp level |  fp+8
        |  retorno   |  fp+4
        |   fp ant   |  fp
        --------------  fp
        |   local1   |  fp-4
        |   local2   |  fp-8
        |    ...     |
        |   localn   |  fp-4*n
*)

structure tigerframe :> tigerframe = struct

open tigertree

type level = int

(* Ver pág 260 ! Explica un toque todo lo que se define acá. *)
datatype access = InFrame of int | InReg of tigertemp.label

(* TODO: PAG 208 *)


val fp = "FP"               (* frame pointer *)
val sp = "SP"               (* stack pointer *)
val rv = "A0"               (* return value  *)
val ra = "RA"               (* return address *)
val zero = "ZERO"
val wSz = 8                 (* word size in bytes *)
val log2WSz = 3             (* base two logarithm of word size in bytes *)
val fpPrev = 0              (* offset (bytes) *)
val fpPrevLev = 8           (* offset (bytes) *)
val argsInicial = 0         (* words *)
val argsOffInicial = 0      (* words *)
val argsGap = wSz           (* bytes *)
val regInicial = 1          (* reg *)
val localsInicial = 0       (* words *)
val localsGap = ~4          (* bytes *)
val specialregs = [ra, fp, sp, zero]
val argregs = ["A0", "A1", "A2", "A3", "A4", "A5", "A6", "A7"]
val callersaves = ["T0", "T1", "T2", "T3", "T4", "T5", "T6"]
val calleesaves = ["S1", "S2", "S3", "S4", "S5", "S6", "S7", "S8", "S9", "S10", "S11"]
val usable_registers = 27   (* All registers (32) except fp, sp, zero, gp, tp. Appel names this as K. *)
val usable_register_list = argregs @ callersaves @ calleesaves

val accessListInicial = [InFrame fpPrevLev]

type frame = {
    name: string,
    formals: bool list,
    locals: bool list,
    actualArg: int ref,
    actualLocal: int ref,
    actualReg: int ref,
    actualArgsLocation : (access list) ref
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
    actualArgsLocation = ref accessListInicial
}

fun name(f: frame) = #name f

fun string(l, s) = l^tigertemp.makeString(s)^"\n"

fun formals({formals=f, actualArgsLocation = a,...}: frame) = !a 

fun maxRegFrame(f: frame) = !(#actualReg f)

fun allocArg (f: frame) escape = 
    case escape of
        true =>
            let
                val ret = (!(#actualArg f)+argsOffInicial)*wSz
                val _ = #actualArg f := !(#actualArg f)+1
                val a = #actualArgsLocation f
                val _ = a := (!a) @ [InFrame ret]              
            in
                InFrame ret
            end
        | false =>
            let
                val a = #actualArgsLocation f
                val temp = tigertemp.newtemp()
                val _ = a := (!a) @ [InReg temp]  
            in 
                InReg temp
            end
        
fun allocLocal (f: frame) escape = 
    case escape of
        true =>
            let val ret = InFrame(!(#actualLocal f)+localsGap)
            in  #actualLocal f:=(!(#actualLocal f)-1); ret end
        | false => InReg(tigertemp.newtemp())

fun exp (InFrame k) fp = MEM(BINOP(PLUS, fp, CONST k))
    | exp (InReg l) fp = TEMP l


fun externalCall(s, l) = CALL(NAME s, l)

fun seqStm [] = raise Fail "seq vacio!"
    | seqStm [s] = s
    | seqStm (x::xs) = tigertree.SEQ (x, seqStm xs)

fun procEntryExit1 (frame:frame,body) = let 
    val argsTemps = List.map TEMP argregs
    val CStemps = List.map (fn r => (TEMP (tigertemp.newtemp()), TEMP(r))) calleesaves
    val argsExps = List.map (fn x => exp x (TEMP fp)) (!(#actualArgsLocation frame))

    val saveCS = List.map MOVE CStemps
    val restoreCS = List.map (MOVE o (fn (t, r) => (r, t))) CStemps
    val moveArgs = List.map MOVE (ListPair.zip (argsExps, argsTemps))
in
    seqStm (saveCS @ moveArgs @ [body] @ restoreCS)
end

fun procEntryExit2 (frame,body) = body @
    [tigerassem.OPER {
        assem="",
        src=specialregs@calleesaves,
        dst=[],
        jump=SOME [] }]

fun procEntryExit3 (frame: frame, body) = {
    prolog = "# PROCEDURE " ^ #name frame ^ "\n"^
             #name frame^":\n",
    body = body,
    epilog = "# END " ^ #name frame ^ "\n\n"
}

end
