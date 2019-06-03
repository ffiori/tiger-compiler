structure tigercodegen :> tigercodegen = struct

    open tigertree
    open tigerassem
    open tigerframe

fun codegen frame stm = (*se aplica a cada funcion*)
    let val ilist = ref ([]:instr list) (*lista de instrucciones que va a ir mutando*)
        fun emit x = ilist := x::(!ilist) (*!ilist es equivalente a *ilist en C y ilist := a es equivalente a *ilist = a en C*)
        
        fun result gen =  let    (*page 205*)
                            val t = tigertemp.newtemp()
                          in 
                            (gen t; t) 
                          end 

        (* munchStm : tigertree.stm -> unit *)            
        fun munchStm s = (*TODO*)
            case s of 
               SEQ (a,b) => (munchStm a; munchStm b) (*204*)
              |tigertree.LABEL l  => emit(LABEL{ assem=l^":\n", lab=l })
              |tigertree.MOVE(tigertree.MEM(e1),e2) => (  (* Store in memory: M[e1] <- e2 *)
                case e1 of
                  (* BINOP(PLUS,e,CONST i) =>     (*  M[e+imm] <- e2 *)
                    emit(OPER{assem = "SW `s0, "^Int.toString i^"(`s1)", 
                              src = [], 
                              dst = [munchExp e,munchExp e2], 
                              jump = NONE})
                  | BINOP(PLUS,CONST i,e) =>   (*  M[imm+e] <- e2 *)
                    emit(OPER{assem = "SW `s0, "^Int.toString i^", `s1", 
                              src = [], 
                              dst = [munchExp e,munchExp e2], 
                              jump = NONE})
                  | CONST(i) =>                 (*M[imm] <- e2*)
                    emit(OPER{assem = "SW `x0, "^Int.toString i^", `s0", 
                              src = [], 
                              dst = [munchExp e2], 
                              jump = NONE}) *)
                   _         =>  (* M[e1] <- e2 . No distinguimos M[e1] <- M[e2] porque nuestra arquitectura no tiene MOVE*)
                    emit(OPER{assem = "SW `s0, 0(`s1)", 
                              src = [munchExp e2,munchExp e1], 
                              dst = [], 
                              jump = NONE})
              )
              |tigertree.MOVE(TEMP t1, e2) =>   (* Store in register: t1 <- e2 *)
                    if (t1=tigerframe.sp orelse t1=tigerframe.fp)
                    then
                        emit(OPER{assem = "ADD "^t1^", `x0, `s0", 
                        src = [munchExp e2], 
                        dst = [], 
                        jump = NONE}) 
                    else
                        emit(OPER{assem = "ADD `d0, `x0, `s0", 
                        src = [munchExp e2], 
                        dst = [t1], 
                        jump = NONE}) 

              | EXP (CALL (e,args)) => emit (OPER{ assem = "CALL 's0\n", (* page 204. CHECK. *)
                                                 src = munchExp(e)::munchArgs(0,args),
                                                 dst = calldefs,
                                                 jump = NONE})

              | EXP _ => raise Fail ("[munchStm] EXP siempre deberia estar compuesto con CALL\n") (* because of canonization *)
              | _ => raise Fail "[munchStm] TODO Emilio \n"
              (*TODO: JUMP, CJUMP*)

      (* munchEXP and muncStm will produce tigerassem.Assem instructions as side effects *)
      (* munchExp : tigertree.exp -> tigertree.temp; returns the temporary in which the result is held *)
      and munchExp e =  (*TODO p. 204 205*)
        let fun  get_code PLUS    i  = if (i) then "ADDI" else "ADD"
                |get_code MINUS   _  = "SUB"
                |get_code MUL     _  = "MUL"
                |get_code DIV     _  = "DIV"
                |get_code AND     i  = if (i) then "ANDI" else "AND"
                |get_code OR      i  = if (i) then "ORI" else "OR"
                |get_code LSHIFT  i  = if (i) then "SLLI" else "SLL"
                |get_code RSHIFT  i  = if (i) then "SRLI" else "SRL"
                |get_code ARSHIFT i  = if (i) then "SRAI" else "SRA"
                |get_code XOR     i  = if (i) then "XORI" else "XOR"
        in
        case e of
          (TEMP t) => t
          | (CONST i) =>
            result (fn r => emit(OPER{assem="LI `d0, "^Int.toString i^"\n",
                                      dst=[r], 
                                      src=[], 
                                      jump=NONE}))
          | (BINOP (b, TEMP sp, CONST i)) =>
            result (fn r => emit(OPER{assem=(get_code b true)^" `d0, "^sp^", "^Int.toString i^"\n", 
                                            dst=[r], 
                                            src=[], 
                                            jump=NONE}))
          | (BINOP (b, TEMP fp, CONST i)) =>
            result (fn r => emit(OPER{assem=(get_code b true)^" `d0, "^fp^", "^Int.toString i^"\n", 
                                            dst=[r], 
                                            src=[], 
                                            jump=NONE}))
          (*| (BINOP(b, CONST i, e1)) =>
            result (fn r => emit(OPER{assem=(get_code b true)^" `d0, `s0, "^    Int.toString i^"\n", 
                                            dst=[r], 
                                            src=[munchExp e1], 
                                            jump=NONE}))
          | (BINOP (b, e1, CONST i)) =>
            result (fn r => emit(OPER{assem=(get_code b true)^" `d0, `s0, "^    Int.toString i^"\n", 
                                            dst=[r], 
                                            src=[munchExp e1], 
                                            jump=NONE})) *) (* revisar imm grandes en const *)
          | (BINOP (b, e1, e2)) =>
            result (fn r => emit(OPER{assem=(get_code b false)^" `d0, `s0, `s1\n", 
                                            dst=[r],
                                            src=[munchExp e1, munchExp e2], 
                                            jump=NONE}))
          
          (*Para acceder a memoria tenemos que usar "LW rd,rs1,imm"  <=> rd <- mem[rs1 + imm]*)
          | (MEM (CONST i)) =>
            result (fn r => emit(OPER{assem="LW `d0, `x0, "^    Int.toString i^"\n", (*x0: hardwired to 0*)
                                            dst=[r], 
                                            src=[], 
                                            jump=NONE}))
          | (MEM (BINOP (PLUS, e1,CONST i))) =>
            result (fn r => emit(OPER{assem="LW `d0, `s0, "^    Int.toString i^"\n", (*x0: hardwired to 0*)
                                            dst=[r], 
                                            src=[munchExp e1], 
                                            jump=NONE}))
          | (MEM (BINOP (PLUS, CONST i, e2))) =>
            result (fn r => emit(OPER{assem="LW `d0, `s0, "^    Int.toString i^"\n", (*x0: hardwired to 0*)
                                            dst=[r], 
                                            src=[munchExp e2], 
                                            jump=NONE}))
          | (MEM e) =>
            result (fn r => emit(OPER{assem="LW `d0, `s0, x0", (*x0: hardwired to 0*)
                                            dst=[r], 
                                            src=[munchExp e], 
                                            jump=NONE}))

          | (NAME _) => tigertemp.newtemp() (* TO DO *)
          | (CALL _) =>  raise Fail ("[munchExp] There should not be CALL nodes outside EXP (canonized tree)") 
          | (ESEQ _ ) =>  raise Fail ("[munchExp] There should not be ESEQ nodes anymore (canonized tree)") 
          | _         =>  raise Fail ("[munchExp] Unknown expression")
        end
            
          

        (* munchArgs : ArgIndex * arg list -> temp list   page 204
        
           munchArgs genetares the code to move all the arguments to their correct positions, in outgoing parameter 
           registers and/or in memory. 

           munchArgs returns a list of all the temporaries that are to be passed to the machine's CALL instructiom
           Even though these temps are never written explicitly in assembly language, they shold be listed as "sources"
           of the instruction, so that liveness analysis can see that teir values need to be kept up to the point of call.

           a CALL is expected to "trash" certain registers (caller-save registers, return address register, and rv)
           The list of calldefs should be listers as "destinations" of the CALL, to that the latter phases of the compiler
           know that something happens to them here
           *)

        and munchArgs(_,_) = [] (* TO DO *)
            
    in
        munchStm stm ; rev(!ilist)
    end

end
