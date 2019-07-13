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
open tigersimpleregalloc
open BasicIO Nonstdio

fun printErr(s: string) = let
    val cross = "\226\157\140"
    val fail = "\027[31m[" ^ cross ^ " FAIL]\027(B\027[m "
    in (output (std_err, fail ^ s); flush_out std_err) end

fun printSucc(s: string) = let
    val check = "\226\156\133"
    val pass = "\027[32m[" ^ check ^ " PASS]\027(B\027[m "
    in print (pass ^ s) end

fun lexstream(is: instream) =
    Lexing.createLexer(fn b => fn n => buff_input is b 0 n);
fun errParsing(lbuf) = (printErr("Error en parsing!("
    ^(makestring(!num_linea))^
    ")["^(Lexing.getLexeme lbuf)^"]\n"); raise Fail "Parse error")
fun main(args) =
    let fun arg(l, s) =
            (List.exists (fn x => x=s) l, List.filter (fn x => x<>s) l)
        val (arbol, l1)         = arg(args, "-arbol")
        val (escapes, l2)       = arg(l1, "-escapes")
        val (ir, l3)            = arg(l2, "-ir")
        val (canon, l4)         = arg(l3, "-canon")
        val (inter, l5)         = arg(l4, "-inter")
        val (simplecolor, l6)   = arg(l5, "-simple")
        val (colordebug, l7)    = arg(l6, "-colordebug")

        val (files, otherargs) = List.partition (String.isSuffix ".tig") l7
        val gccargs = String.concatWith " " otherargs
        val entrada = case files of
              [] => std_in
            | [name] => (open_in name
                        handle _ => raise Fail (name^" does not exist"))
            | _ => raise Fail "Multiple .tig files provided"

        (* 1 - APLICAR LEXER Y PARSER *)
        val lexbuf = lexstream entrada
        val expr = prog Tok lexbuf handle _ => errParsing lexbuf
        val _ = if arbol then tigerpp.exprAst expr else ()
        val _ = printSucc "Parsing completed\n"

        (* 2 - CALCULO DE ESCAPES*)
        val _ = findEscape(expr)
        val _ = if escapes then tigerpp.exprAst expr else ()
        val _ = printSucc "Escapes completed\n"

        (* 3 - CHEQUEO DE TIPOS Y CALCULO DE CODIGO INTERMEDIO *)
        val _ = transProg(expr);
        val fragmentos : tigerframe.frag list = tigertrans.getResult()  
        (* Devuelve datosGlobs. Hay un fragmento (tigertree.stm , tigerframe.frame) por funcion,
           incluido main; Tambien hay un fragmento por string *)
        val _ = if ir then print(tigertrans.Ir(fragmentos)) else ()
        val _ = printSucc "Typechecking and IR complete\n"
        
        (* 4- CANONIZACION *)
        val canonFunction = (tigercanon.traceSchedule o tigercanon.basicBlocks o tigercanon.linearize) (* : tigertree.stm -> tigertree.stm list *)

        fun canonizeProcs (PROC {body=body, frame=frame}) = SOME (canonFunction body, frame)
          | canonizeProcs _ = NONE (* ignores STRING x *)
        fun canonizeStrings (STRING x) = SOME x
          | canonizeStrings _ = NONE (* ignores PROC x *)
        
        (* canonProcs : (stm list, frame) list; por cada funcion en el codigo fuente obtenemos un (tigertree.stm list,frame)  *)
        val canonProcs = List.mapPartial canonizeProcs fragmentos   (* List.mapPartial doesn't save NONE results, and unpacks SOME x into x. Like List.map o List.filter *)
        val canonStrings = List.mapPartial canonizeStrings fragmentos

        val _ =
            if canon
            then print(tigertrans.Ir(
                    List.concat(List.map
                    (fn (bodies,frm) =>
                        List.map (fn b=>PROC {body=b,frame=frm}) bodies)
                    canonProcs
                )))
            else ()
        val _ = printSucc "Canonization complete\n"
        
        val _ = if inter then tigerinterp.inter true canonProcs canonStrings else ()

        (* procesarBody : stm list * frame -> instr list *)
        fun procesarBody (bs,frame) =  (* bs son los statements que componen el body de la función con ese frame *)
            let
                val body_code = List.concat(map (fn b => tigercodegen.codegen frame b) bs) (* Puse concat para aplanarlo como lo hace Appel *)
                val body_code_2 = procEntryExit2(frame,body_code)
                
                val (coalesced_code, temp2regFun) = (* temp2reg is a Splaymap to map temporary registers to actual registers, useful for 'format' function to write final asm file *)
                    if simplecolor
                    then (simpleregalloc frame body_code_2, (fn x=>x))
                    else 
                        let val (code, temp2reg) = tigerregalloc.alloc frame body_code_2 colordebug
                            val _ =
                                if colordebug 
                                then
                                    let val _ = print("COLOREO FINAL \n")
                                        val _ =
                                            Splaymap.app
                                            (fn (temp,reg) => (print(temp^": "^reg^"\n")))
                                            temp2reg
                                    in () end
                        else () 
                        in (code, (fn t=>Splaymap.find(temp2reg,t))) end
                
                val body_code_3 = procEntryExit3(frame, coalesced_code)
            in 
                ( body_code_3, temp2regFun )
            end

        (* functions_code : ({prolog,body,epilog}, allocation) list *)
        val functions_code = List.map procesarBody canonProcs
        val _ = printSucc "Register allocation complete\n"
        
        (* ({prolog,body,epilog}, allocation) list -> (label:string, string:string) -> string -> string -> () *)
        fun create_elf(code, strings, asmfile) = let
            val out_file = TextIO.openOut asmfile
                handle _ => raise Fail ("[tigermain] Problema abriendo "^asmfile)
            fun print_asm txt = TextIO.output(out_file, txt)

            val _ = print_asm ".data\n"
            val _ = List.app (print_asm o tigercodegen.codestring) strings

            val _ = print_asm "\n.text\n"
            val _ = tigerassem.mapAssem print_asm code
            
            val _ = TextIO.closeOut out_file

            val tigerpath = Path.mkAbsolute{path=CommandLine.name(), relativeTo=OS.FileSys.getDir()}
            val runtime = Path.concat (Path.dir tigerpath, "runtime.c")

            val gcc_out = Process.system("riscv64-linux-gcc -static "^runtime^" "^asmfile^" "^gccargs)
            val _ = if not(Process.isSuccess(gcc_out)) then
                raise Fail "problem running gcc" else ()
        in () end
    in
        create_elf(functions_code, canonStrings, "prog.s");
        printSucc "Compilation finished successfully\n"
    end handle Fail s => printErr("Fatal: "^s^"\n")

val _ = main(CommandLine.arguments())
