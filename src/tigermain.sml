open tigerlex
open tigergrm
open tigerescap
open tigerseman
open tigertrans
open tigerframe
open tigerinterp
open tigercodegen
open tigerflow
open tigerliveness
open BasicIO Nonstdio

fun lexstream(is: instream) =
    Lexing.createLexer(fn b => fn n => buff_input is b 0 n);
fun errParsing(lbuf) = (print("Error en parsing!("
    ^(makestring(!num_linea))^
    ")["^(Lexing.getLexeme lbuf)^"]\n"); raise Fail "Parse error")
fun main(args) =
    let fun arg(l, s) =
            (List.exists (fn x => x=s) l, List.filter (fn x => x<>s) l)
        val (arbol, l1)     = arg(args, "-arbol")
        val (escapes, l2)   = arg(l1, "-escapes") 
        val (ir, l3)        = arg(l2, "-ir") 
        val (canon, l4)     = arg(l3, "-canon") 
        val (code, l5)      = arg(l4, "-code") 
        val (flow, l6)      = arg(l5, "-flow") 
        val (inter, l7)     = arg(l6, "-inter") 
        val entrada =
            case l7 of
            [n] => ((open_in n)
                    handle _ => raise Fail (n^" no existe!"))
            | [] => std_in
            | _ => raise Fail "opcio'n dsconocida!"
        val lexbuf = lexstream entrada
        val expr = prog Tok lexbuf handle _ => errParsing lexbuf
        val _ = findEscape(expr)
        val _ = if arbol then tigerpp.exprAst expr else ()
        val _ = transProg(expr);
        val fragmentos = tigertrans.getResult()
        val _ = if ir then print(tigertrans.Ir(fragmentos)) else ()
        
        val canonFunction = (tigercanon.traceSchedule o tigercanon.basicBlocks o tigercanon.linearize) (* : tigertree.stm -> tigertree.stm list *)

        fun canonizeProcs (PROC {body=body, frame=frame}) = SOME (canonFunction body, frame)
          | canonizeProcs _ = NONE (* ignores STRING x *)
        fun canonizeStrings (STRING x) = SOME x
          | canonizeStrings _ = NONE (* ignores PROC x *)
        
        (* canonProcs : (stm list, frame) list *)
        val canonProcs = List.mapPartial canonizeProcs fragmentos   (* List.mapPartial doesn't save NONE results, and unpacks SOME x into x. Like List.map o List.filter *)
        val canonStrings = List.mapPartial canonizeStrings fragmentos
        
        val _ = if inter then tigerinterp.inter true canonProcs canonStrings else ()

        fun showCodegen x = 
            List.app
            (fn y => List.app (fn w => print(tigerassem.format (fn f => f) w)) y)
            x

        (* procesarBody : stm list * frame -> instr list *)
        fun procesarBody (bs,frame) =  (* bs son los statements que componen el body de la función con ese frame *)
            let
                val body_code = List.concat(map (fn b => tigercodegen.codegen frame b) bs) (* Puse concat para aplanarlo como lo hace Appel *)
                val body_code_2 = procEntryExit2(frame,body_code)
                val (code_with_regs, temp2reg) = tigerregalloc.alloc frame body_code_2 (* TODO temp2reg should be a function or a table to map temporary registers to actual registers, useful for formatting function to write final asm file *)
                val body_code_3 = procEntryExit3(frame,code_with_regs)
            in 
                (body_code_3, temp2reg)
            end

        (* functions_code : ({prolog,body,epilog}, allocation) list *)
        val functions_code = List.map procesarBody canonProcs
        val _ = showCodegen (List.map (#body o #1) functions_code)
        
        (* TODO: define format function to convert instrs to a string of assembly code *)

    in
        print "Success\n"
    end handle Fail s => print("Fail: "^s)

val _ = main(CommandLine.arguments())
