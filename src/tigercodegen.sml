structure tigercodegen :> tigercodegen =
	struct
	open tigertree
	open tigerassem
	open tigerframe


fun codegen frame stm = (*se aplica a cada funcion*)
    let val ilist = ref ([]:instr list) (*lista de instrucciones que va a ir mutando*)
        fun emit x = ilist := x::(!ilist) (*!ilist es equivalente a *ilist en C y ilist := a es equivalente a *ilist = a en C*)
        fun results gen = (let val t = tigertemp.newtemp()
                            in (gen t; t) end )

		fun munchExp e = tigertemp.newtemp() (*TODO*)
					
        fun munchStm s = (*TODO*)
            case s of
            (SEQ (a,b) ) => (munchStm a; munchStm b)
            |(tigertree.MOVE (TEMP t1,BINOP(MINUS, TEMP t2, CONST i)) ) =>  (* CONSIDERAR TODOS LOS CASOS QUE USAN FP -SP TODO *)
                if t1 = tigerframe.sp andalso t2 = tigerframe.sp then (*fp y sp no tienen que aparecer en ningun momento en src ni dst porque no pueden ser elegidos para guardar un estado intermedio*)
                    emit(OPER{assem = "MOV SP, SP-"^Int.toString i^"\n", src = [], dst = [], jump = NONE})
                else 
                    emit(OPER{assem = "MOV 'd0, 's0-"^Int.toString i^"\n", src = [t2], dst = [t1], jump = NONE})
				
            | tigertree.MOVE (MEM e1, MEM e2) => (* si no tenemos mem -> mem generamos t<-mem1 seguido de mem2<-t*)
                let val t = tigertemp.newtemp()
                in emit(OPER{assem = "MOV 'd0, MEM['s0]\n", dst=[t], src = [munchExp e2], jump = NONE} ); emit(OPER{assem = "MOV MEM['d0], 's0\n", dst=[munchExp e1], src = [t], jump = NONE} ) 
                end
	in
		munchStm stm ; rev(!ilist)
	end

end
