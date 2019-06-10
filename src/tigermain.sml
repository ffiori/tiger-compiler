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
          
        val canonProcs = List.mapPartial canonizeProcs fragmentos   (* List.mapPartial doesn't save NONE results, and unpacks SOME x into x. Like List.map o List.filter *)
        val canonStrings = List.mapPartial canonizeStrings fragmentos
        
        val _ = if inter then tigerinterp.inter true canonProcs canonStrings else ()

        fun procesarBody (bs,frame) = map (fn b => tigercodegen.codegen frame b) bs

(* Creo que se debería llamar por acá (en procesarBody) a alloc : instr list * frame -> instr list * allocation
* en vez de hacer todo en el main. Tipo alloc debería hacer los grafos y escupir el código
* que se va a usar posta, con los registros ya usados. *)

        (* instr : instr list list list *)
        val instr = List.map procesarBody canonProcs
        
        (* flow_graphs : (flowgraph * tigergraph.node list) list list *)
        val flow_graphs = List.map (fn iis => List.map tigerflow.instrs2graph iis) instr
        
        (* interf_graphs : ((igraph * (tigergraph.node -> tigertemp.temp list)) * tigergraph.node list) list list *)
        val interf_graphs = 
            List.map
            (fn lfs => 
                List.map
                (fn (flow_graph, node_list) => (interferenceGraph flow_graph, node_list))
                lfs)
            flow_graphs
    in
        print "Success\n"
    end handle Fail s => print("Fail: "^s)

val _ = main(CommandLine.arguments())
