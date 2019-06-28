(*
    Frames para riscv (sin registers).

0xffff  
        |    ...     |  s0+8  | 40(sp)
        --------------  s0
        |   fp ant   |  s0    | 32(sp)
        | stat link  |  s0-8  | 24(sp)
        |   return   |  s0-16 | 16(sp)
        |  local 1   |  s0-24 |  8(sp)
        |  local 2   |  sp
        |    ...     |
0x0

*)

structure tigerframe :> tigerframe = struct

open tigertree

type level = int

(* Ver pág 260 ! Explica un toque todo lo que se define acá. *)
datatype access = InFrame of int | InReg of tigertemp.label

(* TODO: PAG 208 *)

fun ppint x = tigerpp.ppint x

val fp = "s0"               (* frame pointer *)
val sp = "sp"               (* stack pointer *)
val rv = "a0"               (* return value  *)
val ra = "ra"               (* return address *)
val zero = "x0"
val wSz = 8                 (* word size in bytes *)
val log2WSz = 3             (* base two logarithm of word size in bytes *)
val fpPrev = 0              (* offset (bytes) *)
val fpPrevLev = ~wSz        (* offset (bytes) *)
val regInicial = 1          (* reg *)
val localsInicial = 0       (* words *)
val localsGap = ~3*wSz      (* bytes *)
val specialregs = [ra, fp, sp, zero]
val argregs = ["a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7"]
val callersaves = ["t0", "t1", "t2", "t3", "t4", "t5", "t6"]
val calleesaves = ["s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8"] (* TODO: borre s9-11 para simpleregallog *)
val usable_register_list = argregs @ callersaves @ calleesaves
val usable_registers = List.length usable_register_list   (* All registers (32) except fp, sp, zero, gp, tp. Appel names this as K. *)
(* COLORTEST please leave this here temporarily, regalloc testing
val usable_registers = 2   (* All registers (32) except fp, sp, zero, gp, tp. Appel names this as K. *)
val usable_register_list = List.take(argregs @ callersaves @ calleesaves,usable_registers)
*)
val accessListInicial = [InFrame fpPrevLev]

type frame = {
    name: string,
    formals: bool list,
    locals: bool list,
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
    actualLocal=ref localsInicial,
    actualReg=ref regInicial,
    actualArgsLocation = ref accessListInicial
}

fun name(f: frame) = #name f

fun string(l, s) = l^tigertemp.makeString(s)^"\n"

fun formals({formals=f, actualArgsLocation = a,...}: frame) = !a 

fun maxRegFrame(f: frame) = !(#actualReg f)
        
fun allocLocal (f: frame) escape = 
    case escape of
        true =>
            let val ret = InFrame(!(#actualLocal f)+localsGap)
            in  #actualLocal f:=(!(#actualLocal f)-wSz); ret end
        | false => InReg(tigertemp.newtemp())

fun allocArg (f: frame) escape =
    let
        val r = allocLocal f escape
        val a = #actualArgsLocation f
        val _ = a := (!a) @ [r]
    in r end

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
    seqStm (saveCS @ moveArgs @ [body] @ restoreCS)  (*COLORTEST: Make this only body*)
end

fun procEntryExit2 (frame,body) = body @
    [tigerassem.OPER {
        assem="",
        src=ra::calleesaves,  (*COLORTEST: Make this []*)
        dst=[],
        jump=SOME [] }]

fun procEntryExit3 (frame: frame, body) = 
let val name = #name frame
    val spOffset = (!(#actualLocal frame) + localsGap)
    val spOffsetA = spOffset - (16 - (~spOffset) mod 16) mod 16
    val save_s0_off = ~spOffsetA - wSz
    val save_a0_off = ~spOffsetA - 2 * wSz
    val save_ra_off = ~spOffsetA - 3 * wSz
in {
    prolog = "# PROCEDURE " ^ name ^ "\n"^
             "\t.align 2\n"^
             "\t.type " ^ name ^ ", @function\n"^
             "\t.globl " ^ name ^ "\n"^
             name^":\n"^
             "\taddi sp, sp, " ^ ppint spOffsetA ^ "\n"^
             "\tsd s0, " ^ ppint save_s0_off ^ "(sp)\n"^
             "\tsd a0, " ^ ppint save_a0_off ^ "(sp)\n"^
             "\tsd ra, " ^ ppint save_ra_off ^ "(sp)\n"^
             "\taddi s0, sp, " ^ ppint save_s0_off ^ "\n",
    body = body,
    epilog = "\tld ra, " ^ ppint save_ra_off ^ "(sp)\n"^
             "\tld s0, " ^ ppint save_s0_off ^ "(sp)\n"^
             "\taddi sp, sp, " ^ ppint (~spOffsetA) ^ "\n"^
             "\tjr ra\n"^
             "# END " ^ name ^ "\n\n"
    }
end

end
