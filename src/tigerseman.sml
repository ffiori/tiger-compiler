structure tigerseman :> tigerseman =
struct

open tigerabs
open tigersres
open tigertrans

type expty = {exp: exp, ty: Tipo}

type venv = (string, EnvEntry) tigertab.Tabla
type tenv = (string, Tipo) tigertab.Tabla

val tab_tipos : (string, Tipo) Tabla = tabInserList(
    tabNueva(),
    [("int", TInt RW), ("string", TString)])

val levelPila: tigertrans.level tigerpila.Pila = tigerpila.nuevaPila1(tigertrans.outermost)
fun pushLevel l = tigerpila.pushPila levelPila l
fun popLevel() = tigerpila.popPila levelPila
fun topLevel() = tigerpila.topPila levelPila

val tab_vars : (string, EnvEntry) Tabla = tabInserList(
    tabNueva(),
    [("print", Func{level=topLevel(), label="print",
        formals=[TString], result=TUnit, extern=true}),
    ("printInt", Func{level=topLevel(), label="printInt",
        formals=[TInt RW], result=TUnit, extern=true}),
    ("flush", Func{level=topLevel(), label="flush",
        formals=[], result=TUnit, extern=true}),
    ("getchar", Func{level=topLevel(), label="getstr",
        formals=[], result=TString, extern=true}),
    ("ord", Func{level=topLevel(), label="ord",
        formals=[TString], result=TInt RW, extern=true}),
    ("chr", Func{level=topLevel(), label="chr",
        formals=[TInt RW], result=TString, extern=true}),
    ("size", Func{level=topLevel(), label="size",
        formals=[TString], result=TInt RW, extern=true}),
    ("substring", Func{level=topLevel(), label="substring",
        formals=[TString, TInt RW, TInt RW], result=TString, extern=true}),
    ("concat", Func{level=topLevel(), label="concat",
        formals=[TString, TString], result=TString, extern=true}),
    ("not", Func{level=topLevel(), label="not",
        formals=[TInt RW], result=TInt RW, extern=true}),
    ("exit", Func{level=topLevel(), label="exitProgram",
        formals=[TInt RW], result=TUnit, extern=true})
    ])

(* Devuelve el tipo al que referencia un sinónimo de tipo *)
fun tipoReal (TTipo s, (env : tenv)) : Tipo =
    (case tabBusca(s , env) of
         NONE => raise Fail ("TipoReal: Ttipo "^s^" no se pudo traducir.")
       | SOME t => t)
  | tipoReal (t, _) = t

fun tiposIguales (TRecord _) TNil = true
  | tiposIguales TNil (TRecord _) = true
  | tiposIguales (TRecord (_, u1)) (TRecord (_, u2)) = (u1=u2)
  | tiposIguales (TArray (_, u1)) (TArray (_, u2)) = (u1=u2)
  | tiposIguales (TInt _) (TInt _) = true
  | tiposIguales (TTipo _) b = raise Fail "No debería pasar! Comparando TTipo (1)"
  | tiposIguales a (TTipo _) = raise Fail "No debería pasar! Comparando TTipo (2)"
  | tiposIguales a b = (a=b)

fun error(s, p) = raise Fail ("Error -- línea "^Int.toString(p)^": "^s^"\n")
fun errorNoLine(s) = raise Fail ("Error: "^s^"\n")

(* Compara tipos y si son compatibles devuelve el tipo más restrictivo
 * al que tipan los dos. *)
fun cmpTipo(TNil, (TRecord r), _) = (TRecord r)
  | cmpTipo((TRecord r), TNil, _) = (TRecord r)
  | cmpTipo((TInt RO), (TInt RW), _) = (TInt RO) 
  | cmpTipo((TInt RW), (TInt RO), _) = (TInt RO) 
  | cmpTipo(t1, t2, nl) = if tiposIguales t1 t2
                          then t1
                          else error("Tipos distintos en cmpTipo!",nl)

fun transExp(venv, tenv) =
    let fun trexp(VarExp v) = trvar(v)
        | trexp(UnitExp _) = {exp=unitExp(), ty=TUnit}
        | trexp(NilExp _)= {exp=nilExp(), ty=TNil}
        | trexp(IntExp(i, _)) = {exp=intExp(i), ty=TInt RW}
        | trexp(StringExp(s, _)) = {exp=stringExp(s), ty=TString}
        | trexp(CallExp({func, args}, nl)) =
            let
                val {formals=targs, result=tresult, level=level, label=label, extern=extern} = case tabBusca(func,venv) of
                    SOME (Func f) => f
                    | _ => error (func^" no es función o no está siendo definida en este batch.",nl)
                val lteargs = List.map trexp args
                val ltexps = List.map (#exp) lteargs
                val ltargs = List.map (#ty) lteargs
                val _ = if List.length targs = List.length ltargs
                        then ()
                        else error("Función "^func^" invocada con una cantidad incorrecta de argumentos!",nl)
                val _ = List.map (fn(x,y) => cmpTipo(x,y,nl)) (ListPair.zip(ltargs,targs))
                        handle Empty => error("Nº de args",nl)
            in {ty=tresult, exp=callExp(label,extern,(tresult = TUnit),level,ltexps)} end
        | trexp(OpExp({left, oper=EqOp, right}, nl)) =
            let
                val {exp= expl, ty=tyl} = trexp left
                val {exp= expr, ty=tyr} = trexp right
            in
                if tiposIguales tyl tyr andalso not (tyl=TNil andalso tyr=TNil) andalso tyl<>TUnit
                then
                    {exp= (if tiposIguales tyl TString
                            then binOpStrExp {left=expl,oper=EqOp,right=expr}
                            else binOpIntRelExp {left=expl,oper=EqOp,right=expr}
                            ), ty=TInt RW}
                else error("Tipos no comparables", nl)
            end
        | trexp(OpExp({left, oper=NeqOp, right}, nl)) =
            let
                val {exp= expl, ty=tyl} = trexp left
                val {exp= expr, ty=tyr} = trexp right
            in
                if tiposIguales tyl tyr andalso not (tyl=TNil andalso tyr=TNil) andalso tyl<>TUnit
                then
                    {exp = (if tiposIguales tyl TString
                            then binOpStrExp {left=expl,oper=NeqOp,right=expr}
                            else binOpIntRelExp {left=expl,oper=NeqOp,right=expr})
                    , ty=TInt RW}
                else error("Tipos no comparables", nl)
            end
        | trexp(OpExp({left, oper, right}, nl)) =
            let
                val {exp= expl, ty=tyl} = trexp left
                val {exp= expr, ty=tyr} = trexp right
            in
                if tiposIguales tyl tyr then
                    case oper of
                        PlusOp => (case tipoReal(tyl,tenv) of TInt _ => {exp=(binOpIntExp {left=expl, oper=oper, right=expr}),ty=TInt RW}
                                                              |_ => error("Error de tipos en +. Debe ser entero.", nl))
                        | MinusOp => (case tipoReal (tyl,tenv) of TInt _ => {exp= (binOpIntExp {left=expl, oper=oper, right=expr}),ty=TInt RW}
                                                                  |_=> error("Error de tipos en -. Debe ser entero.", nl))
                        | TimesOp => (case tipoReal (tyl,tenv) of TInt _ => {exp= (binOpIntExp {left=expl, oper=oper, right=expr}),ty=TInt RW}
                                                                  |_=> error("Error de tipos en *. Debe ser entero.", nl))
                        | DivideOp => (case tipoReal (tyl,tenv) of TInt _ => {exp= (binOpIntExp {left=expl, oper=oper, right=expr}),ty=TInt RW}
                                                                   |_=> error("Error de tipos en /. Debe ser entero.", nl))
                        | LtOp => if tipoReal (tyl,tenv)=TInt RW orelse tipoReal (tyl,tenv)=TInt RO
                                  then {exp= (binOpIntRelExp {left=expl, oper=oper, right=expr}),ty=TInt RW}
                                  else( if tipoReal (tyl,tenv)=TString
                                        then {exp= (binOpStrExp {left=expl, oper=oper, right=expr}),ty=TInt RW}
                                        else error("Error de tipos en <", nl) )
                        | LeOp => if tipoReal (tyl,tenv)=TInt RW orelse tipoReal (tyl,tenv)=TInt RO
                                  then {exp= (binOpIntRelExp {left=expl, oper=oper, right=expr}),ty=TInt RW}
                                  else( if tipoReal (tyl,tenv)=TString
                                        then {exp= (binOpStrExp {left=expl, oper=oper, right=expr}),ty=TInt RW}
                                        else error("Error de tipos en <=", nl) )
                        | GtOp => if tipoReal (tyl,tenv)=TInt RW orelse tipoReal (tyl,tenv)=TInt RO
                                  then {exp = (binOpIntRelExp {left=expl, oper=oper, right=expr}),ty=TInt RW}
                                  else( if tipoReal (tyl,tenv)=TString
                                        then {exp = (binOpStrExp {left=expl, oper=oper, right=expr}),ty=TInt RW}
                                        else error("Error de tipos en >", nl) )
                        | GeOp => if tipoReal (tyl,tenv)=TInt RW orelse tipoReal (tyl,tenv)=TInt RO
                                  then {exp = (binOpIntRelExp {left=expl, oper=oper, right=expr}),ty=TInt RW}
                                  else( if tipoReal (tyl,tenv)=TString
                                        then {exp = (binOpStrExp {left=expl, oper=oper, right=expr}),ty=TInt RW}
                                        else error("Error de tipos en >=", nl) )
                        | _ => raise Fail "No debería pasar! (3)"
                else error("Error de tipos!", nl)
            end
        | trexp(RecordExp({fields, typ}, nl)) =
            let
                (* Traducir cada expresión de fields *)
                val tfields = map (fn (sy,ex) => (sy, trexp ex)) fields

                (* Buscar el tipo *)
                val (tyr, cs) = case tabBusca(typ, tenv) of
                    SOME t => (case tipoReal(t,tenv) of
                        TRecord (cs, u) => (TRecord (cs, u), cs)
                        | _ => error(typ^" no es de tipo record", nl))
                    | NONE => error("Tipo inexistente ("^typ^")", nl)

                (* Verificar que cada campo esté en orden y tenga una expresión del tipo que corresponde.
                 * Los campos del record deben estar en orden cuando se crea una instancia del mismo. Pág 518. *)
                fun verificar _ [] [] = []
                  | verificar _ (c::cs) [] = error("Faltan campos", nl)
                  | verificar _ [] (c::cs) = error("Sobran campos", nl)
                  | verificar n ((s,ref t,_)::cs) ((sy,{exp,ty})::ds) =
                        if s<>sy
                        then error("Error de campo", nl)
                        else if tiposIguales ty t
                             then (exp, n)::(verificar (n+1) cs ds)
                             else error("Error de tipo del campo "^s, nl)
                val lf = verificar 0 cs tfields
            in
                {exp = recordExp lf, ty=tyr} (* exp=lf *)
            end
        | trexp(SeqExp(s, nl)) =
            let val lexti = map trexp s
                val exprs = map (fn{exp=exp, ty} => exp) lexti
                val {exp, ty=tipo} = hd(rev lexti)
            in  { exp = (seqExp(exprs)), ty=tipo } end
        | trexp(AssignExp({var, exp}, nl)) =
            let val {exp=eexp, ty=tyexp} = trexp exp
                val {exp=evar, ty=tyvar} = trvar (var,nl)
                val _ = case tyvar of TInt RO => error("Asignación mal tipada! La variable es ReadOnly!",nl) | _ => ()
                val _ = if tiposIguales tyexp tyvar then () else error("Asignación mal tipada!",nl)
            in {exp = (assignExp{var=evar, exp=eexp}), ty=TUnit} end
        | trexp(IfExp({test, then', else'=SOME else'}, nl)) =
            let val {exp=testexp, ty=tytest} = trexp test
                val {exp=thenexp, ty=tythen} = trexp then'
                val {exp=elseexp, ty=tyelse} = trexp else'
            in
                case (tipoReal(tytest,tenv), tiposIguales tythen tyelse) of
                    (TInt _, true) => {exp = (if tipoReal(tythen,tenv)=TUnit
                                              then ifThenElseExpUnit {test=testexp,then'=thenexp,else'=elseexp}
                                              else ifThenElseExp {test=testexp,then'=thenexp,else'=elseexp}),
                                       ty=cmpTipo(tythen,tyelse,nl)}
                    |_ => error("Error de tipos en ifThenElse o en operacion logica & o |" ,nl)
            end
        | trexp(IfExp({test, then', else'=NONE}, nl)) =
            let val {exp=exptest,ty=tytest} = trexp test
                val {exp=expthen,ty=tythen} = trexp then'
            in
                case (tipoReal(tytest,tenv), tythen) of
                    (TInt _, TUnit) => {exp = (ifThenExp{test=exptest, then'=expthen}), ty=TUnit}
                    |_ => error("Error de tipos en ifThen", nl)
            end
        | trexp(WhileExp({test, body}, nl)) =
            let
                val {exp=texp, ty=tty} = trexp test
                val _ = preWhileForExp()
                val {exp=bexp, ty=bty} = trexp body
                val rta = case (tipoReal(tty,tenv), bty) of
                             (TInt _, TUnit) => {exp = (whileExp {test=texp, body=bexp, lev=topLevel()}), ty=TUnit}
                            |(_, TUnit) => error("Error de tipo en la condición", nl)
                            |(_,_) => error("El cuerpo de un while no puede devolver un valor", nl)
                val _ = postWhileForExp()
            in rta end
        | trexp(ForExp({var, escape=ref escape, lo, hi, body}, nl)) =
            let
                val {ty=tyhi,exp=ehi} = trexp hi
                val {ty=tylo,exp=elo} = trexp lo
                val _ = if (cmpTipo(tyhi,tylo,nl) <> TInt RW 
                          andalso cmpTipo(tyhi,tylo,nl) <> TInt RO)then error("Tipo no entero en rango de for",nl) else ()
                val varEntry = {ty=TInt RO, level=getActualLev(), access=allocLocal (topLevel()) escape}
                val venv' = tabRInserta(var, Var varEntry, venv)
                val _ = preWhileForExp()
                val {ty=tybody,exp=ebody} = transExp (venv',tenv) body
                val _ = if tybody<>TUnit then error("Tipo no unit del cuerpo del for",nl) else ()
                val varexp = simpleVar((#access)varEntry, (#level)varEntry)
                val rta = {exp = (forExp{hi=ehi, lo=elo, body=ebody, var=varexp }), ty=TUnit}
                val _ = postWhileForExp()
            in rta end
        | trexp(LetExp({decs, body}, _)) =
            let
                val (venv',tenv',expdecs) = transDec(venv,tenv,[],decs)
                val {exp=expbody,ty=tybody} = transExp(venv',tenv') body
            in {exp= (seqExp(expdecs@[expbody])), ty=tybody} end
        | trexp(BreakExp nl) =
            (({exp= (breakExp()), ty=TUnit})
                handle Fail s => error(s,nl))
        | trexp(ArrayExp({typ, size, init}, nl)) =
            let
                val expsize = case trexp size of
                                    {ty=TInt _,exp} => exp
                                    | _ => error("Tipo no entero en la cantidad de elementos de un array!",nl)
                val {ty=tyinit,exp=expinit} = trexp init
            in case tabBusca(typ,tenv) of
                SOME (t as (TArray(ref ty,_))) => if tiposIguales tyinit ty
                                                   then {exp = (arrayExp{size=expsize ,init=expinit}), ty=t}
                                                   else error("Mal tipo de los elementos con los que se inicializa el arreglo "^typ,nl)
               |SOME _ => error("Error de tipos, "^typ^" no es array of!.",nl)
               |NONE => error("Error de tipos, "^typ^" no existe.",nl)
            end

        and trvar(SimpleVar s, nl) =
            (case tabBusca(s,venv) of
                 SOME (Var {ty=ty, access=access, level=level}) => {exp = (simpleVar(access,level)), ty=ty}
                |_ => error("No es una variable "^s,nl))
        | trvar(FieldVar(v, s), nl) = (* v.s *)
            let val {exp=exp, ty=tyvar} = trvar(v,nl)
                val (tyfield,index) = case tyvar of
                                        TRecord(lfs,_) => (case List.filter (fn (s',_,_) => s=s') lfs of
                                                              [] => error(s^" no es un campo del record",nl)
                                                              |(_,t',index)::_ => (t',index))
                                        | _ => error("La variable no es un Record!",nl)
            in {exp = (fieldVar(exp,index)) , ty=(!tyfield)} end
        | trvar(SubscriptVar(v, e), nl) = (* v[e] *)
            let val {exp=vexp,ty} = trvar(v,nl)
                val tyfinal = case ty of TArray (ref tinterno, _) => tinterno
                                        | _ => (tigermuestratipos.printTTipos([("ty: ",ty)]); error("Variable no array!",nl))
                val eexp = case trexp e of {exp=eexp,ty=TInt _} => eexp
                                        | _ => error("Se intenta indexar por medio de algo no int",nl)
            in {exp = (subscriptVar(vexp,eexp)) ,ty=tyfinal} end

       (*CHECK: VarDec Cuando se guardan los valores en registros/memoria?? *)
        and transDec (venv,tenv,el,[]) = (venv,tenv,List.rev el) (* el = expression list, inicializaciones de variables de un let. p167 *)
        | transDec (venv,tenv,el,(VarDec ({name,escape=ref escape,typ=NONE,init},nl))::ts) =
            let val {exp=exp,ty=ty} = transExp(venv,tenv) init
                val _ = case ty of TNil => error("Variable "^name^" inicializada en nil sin tipar.",nl)  (* var a := nil, tiene que dar error, test45.tig *)
                                   |_ => ()
                val acc = allocLocal (topLevel()) escape
                val venv' = tabRInserta(name, Var {ty=ty, level=getActualLev(), access=acc}, venv)
                val exp' = varDec(acc,exp)
            in transDec(venv',tenv,exp'::el,ts) end
        | transDec (venv,tenv,el,(VarDec ({name,escape=ref escape,typ=SOME t,init},nl))::ts) =
            let val {exp= exp,ty} = transExp(venv,tenv) init
                val tyasignado = case tabBusca(t,tenv) of (* var x : tyasignado := init, si init es nil, yo quiero que el tipo de x no sea TNil sino tyasignado. *)
                                    NONE => error(t^": tipo no existe",nl)
                                   |SOME ty' => if tiposIguales ty ty'
                                                then ty'
                                                else (tigermuestratipos.printTTipos([("ty: ",ty),("ty': ",ty')]); error("Tipos no coinciden!",nl))
                val acc = allocLocal (topLevel()) escape
                val venv' = tabRInserta(name, Var {ty=tyasignado, level=getActualLev(), access=acc}, venv)
                val exp' = varDec(acc,exp)
            in transDec(venv',tenv,exp'::el,ts) end

        | transDec (venv,tenv,el,(FunctionDec lf)::ts) = (* lf = lista de funciones, es un batch de declaraciones de funciones *)
            let
                (* 1 - chequear si hay nombres duplicados en este batch *)
                val _ = List.foldr
                        (fn (({name,params,result,body},pos),newfuncs') =>
                            if tabEsta(name,newfuncs')
                            then error("Función "^name^" definida dos veces en el mismo batch!",pos)
                            else tabInserta(name,(),newfuncs'))
                        (tabNueva())
                        lf

                (* 2 - Aumentar venv con los tipos de las funciones definidas en este batch *)

                (* 2.a - Para cada nueva funcion, crear la entrada que se va a meter en la tabla *)
                (* lpr = lista de (name,Func x) *)
                val lpr = List.map
                          ( fn ({name,params,result,body},pos) =>
                              (* Crear un label unico para la funcion. Podemos usar el nro de linea por si se repiten nombres *)
                              let val label = if name="_tigermain" then name else name^"_"^tigertemp.newlabel()^"_"^(Int.toString pos)

                              in (name,Func{formals = List.map (fn {name=_,escape=_,typ=typ} => transTy(tenv,typ,pos)) params,
                                            result = case result of
                                                        NONE => TUnit
                                                        |SOME tr => (case tabBusca(tr,tenv) of
                                                                        NONE => error("Tipo de retorno "^tr^" no existe!",pos)
                                                                        |SOME t => t),
                                            label = label,
                                            level = newLevel{parent=topLevel(),
                                                             name=label,
                                                             formals=List.map (fn {name=_,escape=ref escape,typ=_} => escape) params},
                                            extern = false})
                              end )
                          lf

                (* 2.b - Agregar las entradas a venv, pisando las variables del mismo nombre (p515 name spaces). *)
                val venv' = List.foldr
                            ( fn ((name,funcEntry),env) => tabRInserta(name,funcEntry,env) )
                            venv
                            lpr

                (* 3 - Aumentar el entorno de variables con los argumentos y calcular CI *)
                val ci =
                    List.map

                    (fn (func as ({body,name,params,result},pos)) =>
                        let
                            val _ = preFunctionDec() (* Hace pushSalida(NONE); para los breaks, y actualLevel++; *)
                            val (level,tyresult) =
                                case tabBusca(name,venv') of
                                    SOME (Func{result,level,...}) => (level,result)
                                    |_ => error("No debería pasar, función "^name,pos)
                            val _ = pushLevel level

                            val venv'' =
                                List.foldl
                                (fn ({name,escape,typ},env) =>
                                    tabRInserta(name, Var {ty = transTy(tenv,typ,pos),
                                                           access = tigertrans.allocArg (topLevel()) (!escape),
                                                           level = getActualLev()}, env))
                                venv'
                                params

                            val {exp=exp,ty=tybody} = transExp(venv'',tenv) body

                            val _ = if tiposIguales tybody tyresult
                                    then ()
                                    else error("Tipo de retorno de la funcion y el tipo de su body no coinciden",pos)

                            val _ = popLevel()
                            val _ = postFunctionDec() (* Hace popSalida(); actualLevel--; *)
                        in
                            functionDec(exp, level, (tyresult = TUnit))
                        end)

                   lf

                val codint = List.foldl (fn (exp,els) => exp::els) el ci

            in transDec(venv',tenv,codint,ts) end
        | transDec (venv,tenv,el,(TypeDec lt)::ts) = (*lt: batch de declaraciones de tipos*)
            let val sortedNames = Listsort.sort
                                  (fn (({name=x,ty=_},_), ({name=y,ty=_},_)) => if x<y then LESS else (if x>y then GREATER else EQUAL))
                                  lt
                val _ = List.foldr (* Chequea que no hay dos seguidos iguales en sortedNames *)
                        (fn (t1 as ({name=n1,ty=_},posx), ({name=n2,ty=_},_)) => if n1=n2 then error("Se definió dos veces el tipo "^n1^" en un mismo batch.",posx) else t1)
                        ({name="",ty=NameTy ""},0) (* Invento un tipo con nombre "" que no va a ser igual a ninguno de los que se definan. *)
                        sortedNames
                val ltsinpos = List.map (fn (x,pos) => x) lt
                val tenv' = tigertopsort.fijaTipos ltsinpos tenv handle Ciclo => errorNoLine("Ciclo en la declaración de tipos!")
            in transDec(venv,tenv',el,ts) end

        (* transTy (tenv, ty : ty, nl) : Tipo *)
        and transTy(tenv,NameTy tname,nl) =
            (case tabBusca(tname,tenv) of
              NONE => error("Tipo "^tname^" desconocido",nl)
             |SOME t => t )
        | transTy(tenv,ArrayTy tname,nl) =
            (case tabBusca(tname,tenv) of
              NONE => error("Tipo "^tname^" desconocido",nl)
             |SOME t => TArray (ref t,ref ()) ) (* Ese ref () no importa porque después se define bien en el toposort, igual que el int constante de abajo *)
        | transTy(tenv,RecordTy flist,nl) =
            let val flist' = List.map (fn {name,escape,typ} => (name,ref (transTy(tenv,typ,nl)),123456)) flist
            in TRecord (flist',ref ()) end
    in trexp end

fun transProg ex =
    let val main = LetExp({decs=[FunctionDec[({name="_tigermain", params=[], result=SOME "int", body=ex}, 0)]], body=UnitExp 0}, 0)
        (* val _ = ppvenv tab_vars *)
        (* val _ = pptenv tab_tipos *)
    in transExp(tab_vars, tab_tipos) main; () end

end
