open tigerlex
open tigergrm
open tigerescap
open tigerseman
open tigertrans
open tigerframe
open tigerinterp
open tigercodegen
open BasicIO Nonstdio

fun lexstream(is: instream) =
	Lexing.createLexer(fn b => fn n => buff_input is b 0 n);
fun errParsing(lbuf) = (print("Error en parsing!("
	^(makestring(!num_linea))^
	")["^(Lexing.getLexeme lbuf)^"]\n"); raise Fail "Parse error")
fun main(args) =
	let	fun arg(l, s) =
			(List.exists (fn x => x=s) l, List.filter (fn x => x<>s) l)
		val (arbol, l1)		= arg(args, "-arbol")
		val (escapes, l2)	= arg(l1, "-escapes") 
		val (ir, l3)		= arg(l2, "-ir") 
		val (canon, l4)		= arg(l3, "-canon") 
		val (code, l5)		= arg(l4, "-code") 
		val (flow, l6)		= arg(l5, "-flow") 
		val (inter, l7)		= arg(l6, "-inter") 
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
    val procs = List.filter
                (fn frag =>
                    case frag of
                        PROC _ => true
                      | STRING _ => false
                )
                fragmentos
    val strings = List.filter
                  (fn frag =>
                      case frag of
                          PROC _ => false
                        | STRING _ => true
                  )
                  fragmentos
    val canonProcs = List.map (fn PROC {body=body, frame=frame} => ( (canonFunction body, frame))) procs
    val canonStrings = List.map (fn STRING x => x) strings
    val _ = if inter then tigerinterp.inter true canonProcs canonStrings else ()

		fun procesarBody (bs,frame) = map (fn b => tigercodegen.codegen frame b ) bs

		val instr = List.map procesarBody canonProcs 


	in
		print "Success\n"
	end	handle Fail s => print("Fail: "^s)

val _ = main(CommandLine.arguments())
