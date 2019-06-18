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

        fun get_relop_assem i = case i of
            EQ => "BEQ `s0, `s1"
          | NE => "BNE `s0, `s1"
          | LT => "BLT `s0, `s1"
          | GT => "BLT `s1, `s0"
          | LE => "BGE `s1, `s0"
          | GE => "BGE `s0, `s1"
          | ULT => "BLTU `s0, `s1"
          | ULE => "BGEU `s1, `s0"
          | UGT => "BLTU `s1, `s0"
          | UGE => "BGEU `s0, `s1"

        (* safeMunchStm : tigertree.stm -> unit
           slow but safe implementations *) 
        fun safeMunchStm s =
          case s of
            tigertree.MOVE(tigertree.MEM(e1),e2) =>
                emit(OPER{assem = "SD `s0, 0(`s1)\n", 
                          src = [munchExp e2,munchExp e1], 
                          dst = [], 
                          jump = NONE})
            | _ => raise Fail ("[safeMunchStm] unknown thing")

        (* munchStm : tigertree.stm -> unit *)            
        and munchStm s = (*TODO*)
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
                   _         =>  safeMunchStm s 

              )
              |tigertree.MOVE(TEMP t1, e2) =>   (* Store in register: t1 <- e2 *)
                    if (t1=tigerframe.sp orelse t1=tigerframe.fp)
                    then
                        emit(OPER{assem = "ADD "^t1^", x0, `s0\n", 
                        src = [munchExp e2], 
                        dst = [], 
                        jump = NONE}) 
                    else
                        emit(OPER{assem = "ADD `d0, x0, `s0\n", 
                        src = [munchExp e2], 
                        dst = [t1], 
                        jump = NONE}) 

              | EXP (CALL (NAME n,args)) => emit (OPER{ assem = "JAL "^n^"\n", (* page 204. CHECK. *)
                                                 src = munchArgs(args),
                                                 dst = [ra],
                                                 jump = NONE})

              | EXP _ => raise Fail ("[munchStm] EXP siempre deberia estar compuesto con CALL ") (* because of canonization *)
              | JUMP (NAME n, l) => emit (OPER{ assem = "J "^n^"\n",
                                                 src = [],
                                                 dst = [],
                                                 jump = SOME l})
              | CJUMP(oper,e1,e2,t,f) => ( emit (OPER{ assem = (get_relop_assem oper)^", "^t^"\n",
                                                 src = [munchExp e1, munchExp e2],
                                                 dst = [],
                                                 jump = SOME [t,f]})) (* por canon este cjump tiene a continuacion el label f *)
              | _ => emit(OPER{assem = "MISSING\n", 
                        src = [], 
                        dst = [], 
                        jump = NONE})

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
          | (BINOP (b, TEMP "sp", CONST i)) =>
            result (fn r => emit(OPER{assem=(get_code b true)^" `d0, "^sp^", "^Int.toString i^"\n", 
                                            dst=[r], 
                                            src=[], 
                                            jump=NONE}))
          | (BINOP (b, TEMP "fp", CONST i)) =>
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
          (* | (MEM (CONST i)) =>
            result (fn r => emit(OPER{assem="LD `d0, `x0, "^    Int.toString i^"\n", (*x0: hardwired to 0*)
                                            dst=[r], 
                                            src=[], 
                                            jump=NONE}))
          | (MEM (BINOP (PLUS, e1,CONST i))) =>
            result (fn r => emit(OPER{assem="LD `d0, `s0, "^    Int.toString i^"\n", (*x0: hardwired to 0*)
                                            dst=[r], 
                                            src=[munchExp e1], 
                                            jump=NONE}))
          | (MEM (BINOP (PLUS, CONST i, e2))) =>
            result (fn r => emit(OPER{assem="LD `d0, `s0, "^    Int.toString i^"\n", (*x0: hardwired to 0*)
                                            dst=[r], 
                                            src=[munchExp e2], 
                                            jump=NONE})) *) (* revisar imm grandes en const *)
          | (MEM e) =>
            result (fn r => emit(OPER{assem="LD `d0, 0(`s0)\n",
                                            dst=[r], 
                                            src=[munchExp e], 
                                            jump=NONE}))

          | (NAME _) => raise Fail ("[munchExp] There should not be NAME nodes outside of CALL (canonized tree)") 
          | (CALL _) =>  raise Fail ("[munchExp] There should not be CALL nodes outside EXP (canonized tree)") 
          | (ESEQ _ ) =>  raise Fail ("[munchExp] There should not be ESEQ nodes anymore (canonized tree)") 
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

        and munchArgs(args) = List.map munchExp args (* TO DO *)
            
    in
        munchStm stm ; rev(!ilist)
    end

end
