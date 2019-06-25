structure tigertrans :> tigertrans = struct

open tigerframe
open tigertree
open tigertemp
open tigerabs

exception breakexc
exception divCero

type level = {parent:frame option, frame: frame, level: int}
type access = tigerframe.access

type frag = tigerframe.frag
val fraglist = ref ([]: frag list)

val actualLevel = ref ~1 (* _tigermain debe tener level = 0. *)
fun getActualLev() = !actualLevel

val outermost: level = {parent=NONE,
    frame=newFrame{name="_tigermain", formals=[]}, level=getActualLev()}
fun newLevel{parent={parent, frame, level}, name, formals} =
    {
    parent=SOME frame,
    frame=newFrame{name=name, formals=formals},
    level=level}
fun allocArg{parent, frame, level} b = tigerframe.allocArg frame b
fun allocLocal{parent, frame, level} b = tigerframe.allocLocal frame b
fun formals{parent, frame, level} = tigerframe.formals frame

datatype exp =
    Ex of tigertree.exp
    | Nx of tigertree.stm
    | Cx of label * label -> tigertree.stm

fun seq [] = EXP (CONST 0)
    | seq [s] = s
    | seq (x::xs) = SEQ (x, seq xs)

fun unEx (Ex e) = e
    | unEx (Nx s) = ESEQ(s, CONST 0)
    | unEx (Cx cf) =
    let
        val r = newtemp()
        val t = newlabel()
        val f = newlabel()
    in
        ESEQ(seq [MOVE(TEMP r, CONST 1),
            cf (t, f),
            LABEL f,
            MOVE(TEMP r, CONST 0),
            LABEL t],
            TEMP r)
    end

fun unNx (Ex e) = EXP e
    | unNx (Nx s) = s
    | unNx (Cx cf) =
    let
        val t = newlabel()
        val f = newlabel()
    in
        seq [cf(t,f),
            LABEL t,
            LABEL f]
    end

fun unCx (Nx s) = raise Fail ("Error (UnCx(Nx..))")
    | unCx (Cx cf) = cf
    | unCx (Ex (CONST 0)) =
    (fn (t,f) => JUMP(NAME f, [f]))
    | unCx (Ex (CONST _)) =
    (fn (t,f) => JUMP(NAME t, [t]))
    | unCx (Ex e) =
    (fn (t,f) => CJUMP(NE, e, CONST 0, t, f))

fun Ir(e) =
    let fun aux(Ex e) = tigerit.tree(EXP e)
        | aux(Nx s) = tigerit.tree(s)
        | aux _ = raise Fail "bueno, a completar!"
        fun aux2(PROC{body, frame}) = aux(Nx body)
        | aux2(STRING(l, "")) = l^":\n"
        | aux2(STRING("", s)) = "\t"^s^"\n"
        | aux2(STRING(l, s)) = l^":\t"^s^"\n"
        fun aux3 [] = ""
        | aux3(h::t) = (aux2 h)^(aux3 t)
    in  aux3 e end
fun nombreFrame frame = print(".globl " ^ tigerframe.name frame ^ "\n")

(* While y for necesitan la u'ltima etiqueta para un break *)
local
    val salidas: label option tigerpila.Pila = tigerpila.nuevaPila1 NONE
in
    val pushSalida = tigerpila.pushPila salidas
    fun popSalida() = tigerpila.popPila salidas
    fun topSalida() =
        case tigerpila.topPila salidas of
        SOME l => l
        | NONE => raise Fail "break incorrecto!"
end

val datosGlobs = ref ([]: frag list)
fun procEntryExit{level: level, body} =
    let val body' = PROC{frame= #frame level, body=unNx body}
    in  datosGlobs:=(!datosGlobs@[body']) end
fun getResult() = !datosGlobs

fun stringExp(s: string) =
    let val l = newlabel()
        val _ = datosGlobs:=(!datosGlobs @ [STRING(l, s)])
    in  Ex(NAME l) end

fun preFunctionDec() =
    (pushSalida(NONE); (* para que no haya break sueltos*)
    actualLevel := !actualLevel+1)

(*val functionDec : exp * level * bool -> exp *)
fun functionDec(e, l, proc) =
    let val body =
                if proc then unNx e
                else MOVE(TEMP rv, unEx e)
        val body' = procEntryExit1(#frame l, body)
        val () = procEntryExit{body=Nx body', level=l}
    in  Ex(CONST 0) end

fun postFunctionDec() =
    (popSalida(); actualLevel := !actualLevel-1)

fun unitExp() = Ex (CONST 0)

fun nilExp() = Ex (CONST 0)

fun intExp i = Ex (CONST i)

fun simpleVar(acc, nivel) =
    case acc of
         InReg r => Ex (TEMP r)
        |InFrame k =>
            let fun  aux 0 = TEMP fp
                    |aux n = MEM(BINOP(PLUS,CONST fpPrevLev,aux (n-1)))
            in Ex(MEM(BINOP(PLUS,CONST k,aux(!actualLevel-nivel)))) end

fun varDec(acc,exp) = Nx (MOVE(
                           unEx(simpleVar(acc, getActualLev())),
                           unEx(exp)
                          )
                      )

fun fieldVar(var, field) =
    let val r = unEx var
        val t = newtemp()
    in (* Debemos chequear que r no es nil en tiempo de ejecución *)
        Ex(ESEQ(seq [MOVE(TEMP t,r),
                  EXP(CALL(NAME "_checkNil",[TEMP t]))],
             MEM(BINOP(PLUS,TEMP t,CONST (wSz*field)))))
    end

fun subscriptVar(arr, ind) =
let
    val a = unEx arr
    val i = unEx ind
    val ra = newtemp()
    val ri = newtemp()
in
    Ex( ESEQ(seq[MOVE(TEMP ra, a),
                 MOVE(TEMP ri, i),
                 EXP(externalCall("_checkIndexArray", [TEMP ra, TEMP ri]))],  (*Does it check for nil?*)
                 MEM(BINOP(PLUS, TEMP ra,
                     BINOP(MUL, TEMP ri, CONST tigerframe.wSz)))))
end

(*fun recordExp [] = (* caso {} vale! *)  TODO  *)
fun recordExp l = (* l es (exp * int) list *) (* pág 175 o 164 *)
let
    val tsz = newtemp()
    val r = newtemp()
    fun alojar [] = [] (* Crea una lista de operaciones que van alojando las expresiones de los fields *)
       |alojar ((f,n)::fs) = (MOVE(MEM(BINOP(PLUS,TEMP r,CONST (n*wSz))), unEx f))::(alojar fs)
in
    Ex (ESEQ(seq ([MOVE(TEMP tsz, CONST ((List.length l)*wSz)),
                   EXP(externalCall("_allocRecord",[TEMP tsz])),  (* Alojo espacio para el record *)
                   MOVE(TEMP r, TEMP rv)] @ (alojar l)),
             TEMP r))
end

fun arrayExp{size, init} =
let
    val s = unEx size
    val i = unEx init
    val (ti,ts) = (newtemp(),newtemp())
    val t = newtemp()
in
    Ex (ESEQ(seq [MOVE(TEMP ts,s),
                  MOVE(TEMP ti,i),
                  EXP(externalCall("_initArray", [TEMP ts, TEMP ti])),
                  MOVE(TEMP t,TEMP rv)], TEMP t))
end

(* p.166 *)
fun callExp (name,extern:bool,isproc:bool,lev:level,ls:exp list) =
    let
        (* Calcular el static link *)
        fun saltar 0 = TEMP fp
        | saltar n = MEM (BINOP (PLUS, saltar(n-1), CONST fpPrevLev))

        val fp_loc = if (#level lev) = getActualLev()
                     then MEM (BINOP (PLUS, TEMP fp, CONST fpPrevLev))
                     else if (#level lev) > getActualLev()
                          then saltar (getActualLev() - (#level lev) + 1)
                          else TEMP fp

        (* Calcular la localizacion de cada arg + cod intemedio si es necesario *)
        val locAndci = List.map
                       (fn(exp) => case exp of
                            Ex(CONST s) => (CONST s, [])
                            |Ex(NAME s) => (NAME  s, [])
                            |Ex(TEMP s) => (TEMP  s, [])
                            | _         => (let val t' = newtemp()
                                            in (TEMP t', [MOVE(TEMP t', unEx exp)])
                                            end)
                       )
                       ls

        val ta = List.rev (List.map (fn (a,b) => a) locAndci)
        val ci = List.concat (List.map (fn (a,b) => b) locAndci)
        val args = if extern then ta else fp_loc::ta (* extern functions do not require static link *)
    in
        if isproc
        then Nx (seq (ci@[EXP(CALL(NAME name,args))]))
        else let
                val tmp = newtemp()
             in (* page 178: assign each RV into a fresh temporary register *)
                Ex (ESEQ ( seq(ci@[EXP(CALL(NAME name, args)), MOVE(TEMP tmp, TEMP rv)]), TEMP tmp))
             end
    end

fun letExp ([], body) = Ex (unEx body)
 |  letExp (inits, body) = Ex (ESEQ(seq inits,unEx body))

fun breakExp() =
    let val l = topSalida() handle Empty => raise Fail "break sin iteración! (no deberia pasar, error interno del compilador)"
    in Nx (JUMP(NAME l,[l])) end

fun seqExp ([]:exp list) = Nx (EXP(CONST 0))
    | seqExp (exps:exp list) =
        let
            fun unx [e] = []
                | unx (s::ss) = (unNx s)::(unx ss)
                | unx[] = []
        in
            case List.last exps of
                Nx s =>
                    let val unexps = map unNx exps
                    in Nx (seq unexps) end
                | Ex e => Ex (ESEQ(seq(unx exps), e))
                | cond => Ex (ESEQ(seq(unx exps), unEx cond))
        end

(* Para lidiar con los breaks *)
fun preWhileForExp() = pushSalida(SOME(newlabel()))
fun postWhileForExp() = (popSalida(); ())

fun whileExp {test: exp, body: exp, lev:level} =
let
    val cf = unCx test
    val expb = unNx body
    val (l1, l2, l3) = (newlabel(), newlabel(), topSalida())
in
    Nx (seq[LABEL l1,
        cf(l2,l3),
        LABEL l2,
        expb,
        JUMP(NAME l1, [l1]),
        LABEL l3])
end

fun forExp {lo, hi, var, body} =
    let val var' = unEx var
        val (l1,l2,sal) = (newlabel(), newlabel(), topSalida())
    in Nx (seq (case hi of
        Ex (CONST n) => (* Se puede simplificar sacando el primer caso e incluyendolo en el segundo directamente. *)
            if n<valOf(Int.maxInt) (* Parche para el caso en que n=maxInt *)
            then [ MOVE(var',unEx lo),
                   JUMP(NAME l2,[l2]),
                   LABEL l1, unNx body,
                   MOVE(var',BINOP(PLUS,var',CONST 1)),
                   LABEL l2, CJUMP(GT,var',CONST n,sal,l1),
                   LABEL sal ]
            else [ MOVE(var',unEx lo), (* Si n=maxInt entonces debo hacer al menos una
                                           iteración y tener como condición de salida que var'=n,
                                           ya que nunca va a ser mayor. *)
                   LABEL l2, unNx body, CJUMP(EQ,var',CONST n,sal,l1),
                   LABEL l1, MOVE(var',BINOP(PLUS,var',CONST 1)),
                   JUMP(NAME l2,[l2]),
                   LABEL sal ]
       | _ =>
            let val t = newtemp()
            in [ MOVE(var',unEx lo),
                 MOVE(TEMP t, unEx hi),
                 CJUMP(LE,var',TEMP t,l2,sal),
                 LABEL l2, unNx body,
                 CJUMP(EQ,TEMP t,var',sal,l1), (* Parche para el caso en que hi=maxInt *)
                 LABEL l1, MOVE(var',BINOP(PLUS,var',CONST 1)),
                 JUMP(NAME l2,[l2]),
                 LABEL sal ]
            end ))
    end

fun ifThenExp{test, then'} =
    let val cj = unCx test
        val (l1,l2) = (newlabel(),newlabel())
    in Nx (seq [cj(l1,l2),
                LABEL l1, unNx then',
                LABEL l2]) end

fun ifThenElseExp {test,then',else'} =
    let val cj = unCx test
        val (l1,l2,l3) = (newlabel(),newlabel(),newlabel())
        val temp = newtemp()
    in Ex (ESEQ (seq [cj(l1,l2),
                      LABEL l1, MOVE(TEMP temp,unEx then'),
                      JUMP (NAME l3,[l3]),
                      LABEL l2, MOVE(TEMP temp, unEx else'),
                      LABEL l3], TEMP temp)) end

fun ifThenElseExpUnit {test,then',else'} =
    let val cj = unCx test
        val (l1,l2,l3) = (newlabel(),newlabel(),newlabel())
    in Nx (seq [cj(l1,l2),
                LABEL l1, unNx then',
                JUMP (NAME l3,[l3]),
                LABEL l2, unNx else',
                LABEL l3]) end

fun assignExp{var, exp} =
let
    val v = unEx var
    val vl = unEx exp
in
    Nx (MOVE(v,vl))
end

fun binOpIntExp {left, oper, right} =
    let val l = unEx left
        val r = unEx right
        val oper' = case oper of
                        PlusOp => PLUS
                       |MinusOp => MINUS
                       |TimesOp => MUL
                       |DivideOp => DIV
                       |_ => raise Fail "Operador raro..."
    in
        Ex(BINOP(oper',l,r))
    end

fun binOpIntRelExp {left,oper,right} =
    let val l = unEx left
        val r = unEx right
        val rta = newtemp()
        val (t,f) = (newlabel(),newlabel())
        val oper' = case oper of
                        EqOp => EQ
                       |NeqOp => NE
                       |LtOp => LT
                       |GtOp => GT
                       |LeOp => LE
                       |GeOp => GE
                       |_ => raise Fail "Operador raro rel..."
    in
        Ex(ESEQ(seq[MOVE(TEMP rta,CONST 0),
                    CJUMP(oper',l,r,t,f),
                    LABEL t, MOVE(TEMP rta,CONST 1),
                    LABEL f],
                TEMP rta))
    end

fun binOpStrExp {left,oper,right} =
    let
        val l = unEx left
        val r = unEx right
        val (tl,tr,rta) = (newtemp(),newtemp(),newtemp())
        val (t,f) = (newlabel(),newlabel())
        val oper' = case oper of
                        EqOp => EQ
                       |NeqOp => NE
                       |LtOp => LT
                       |GtOp => GT
                       |LeOp => LE
                       |GeOp => GE
                       |_ => raise Fail "Operador raro strrel..."
    in
        Ex (ESEQ(seq [MOVE(TEMP tl,l),
                      MOVE(TEMP tr,r),
                      EXP(externalCall("_stringCompare", [TEMP tl, TEMP tr])),
                      MOVE(TEMP rta,CONST 0),
                      CJUMP(oper',TEMP rv,CONST 0,t,f),
                      LABEL t, MOVE(TEMP rta,CONST 1),
                      LABEL f],
                 TEMP rta))
    end

end
