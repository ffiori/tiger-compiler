structure tigercodegen :> tigercodegen = struct

    open tigertree
    open tigerassem
    open tigerframe

fun ppint x = tigerpp.ppint x

fun stringLen s =
    let fun aux[] = 0
        | aux(#"\\":: #"x"::_::_::t) = 1+aux(t)
        | aux(_::t) = 1+aux(t)
    in  aux(explode s) end

fun codestring (lab, str) =
  let val len = stringLen str
  in
    ".size " ^ lab ^ ", " ^ ppint (len + 8) ^ "\n"^
    lab ^ ":\n"^
    "\t.dword " ^ ppint len ^ "\n"^
    "\t.ascii \"" ^ str ^ "\"\n"
  end

fun codegen frame stm = (*se aplica a cada funcion*)
    let val ilist = ref ([]:instr list) (*lista de instrucciones que va a ir mutando*)
        fun emit x = ilist := x::(!ilist) (*!ilist es equivalente a *ilist en C y ilist := a es equivalente a *ilist = a en C*)
        
        val imm_max = 2048 (* TODO check, varies per instruction *)
        val imm_min = ~2048
        fun valid_imm i = i < imm_max andalso i > imm_min

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

        fun binop_supports_imm e = case e of
            MINUS => false
          | MUL => false
          | DIV => false
          | _ => true

        fun binop_commutes e = case e of
            MINUS   => false
          | DIV     => false
          | LSHIFT  => false
          | RSHIFT  => false
          | ARSHIFT => false
          | _       => true

        fun get_binop_code e i = case e of
            PLUS    => if (i) then "ADDI" else "ADD"
          | MINUS   => "SUB"
          | MUL     => "MUL"
          | DIV     => "DIV"
          | AND     => if (i) then "ANDI" else "AND"
          | OR      => if (i) then "ORI" else "OR"
          | LSHIFT  => if (i) then "SLLI" else "SLL"
          | RSHIFT  => if (i) then "SRLI" else "SRL"
          | ARSHIFT => if (i) then "SRAI" else "SRA"
          | XOR     => if (i) then "XORI" else "XOR"

        (* safeMunchStm : tigertree.stm -> unit
           slow but safe implementations *) 
        fun safeMunchStm s =
          case s of
            tigertree.MOVE(tigertree.MEM(e1),e2) =>
                emit(OPER{assem = "SD `s0, 0(`s1)\n", 
                          src = [munchExp e2,munchExp e1], 
                          dst = [], 
                          jump = NONE})
            |  tigertree.MOVE(TEMP t1, e2) =>
                emit(MOVE{assem = "ADD `d0, x0, `s0\n",
                          src = munchExp e2,
                          dst = t1})
            | _ => raise Fail ("[safeMunchStm] unknown thing")

        (* munchStm : tigertree.stm -> unit *)            
        and munchStm s =
            case s of 
               SEQ (a,b) => (munchStm a; munchStm b) (*204*)
              |tigertree.LABEL l  => emit(LABEL{ assem=l^":\n", lab=l })
              |tigertree.MOVE(tigertree.MEM(e1),e2) => (  (* Store in memory: M[e1] <- e2 *)
                case e1 of
                  BINOP(PLUS,e,CONST i) =>     (*  M[e+imm] <- e2 *)
                    if valid_imm i then
                    emit(OPER{assem = "SD `s0, "^ppint i^"(`s1)\n", 
                              src = [munchExp e2,munchExp e], 
                              dst = [], 
                              jump = NONE})
                    else safeMunchStm s
                  | BINOP(PLUS,CONST i,e) =>   (*  M[imm+e] <- e2 *)
                    if valid_imm i then
                    emit(OPER{assem = "SD `s0, "^ppint i^"(`s1)\n", 
                              src = [munchExp e2,munchExp e], 
                              dst = [], 
                              jump = NONE})
                    else safeMunchStm s
                  | CONST(i) =>                 (*M[imm] <- e2*)
                    if valid_imm i then
                    emit(OPER{assem = "SD `s0, "^ppint i^"(x0)\n", 
                              src = [munchExp e2], 
                              dst = [], 
                              jump = NONE})
                    else safeMunchStm s
                  | _         =>  safeMunchStm s 

              )
              |tigertree.MOVE(TEMP t1, CONST 0) =>
                    emit(MOVE{assem = "ADD `d0, x0, `s0\n",
                              src = zero,
                              dst = t1})
              |tigertree.MOVE(TEMP t1, CONST i) =>   (* Store in register: t1 <- i *)
                    if valid_imm i then
                    emit(OPER{assem = "LI `d0, "^ppint i^"\n",
                              src = [],
                              dst = [t1],
                              jump = NONE})
                    else safeMunchStm s
              |tigertree.MOVE(TEMP t1, e2) =>   (* Store in register: t1 <- e2 *)
                    safeMunchStm s

              (* CHECK: call podria pisar t1 *)
              | EXP (CALL (NAME n,args)) => emit (OPER{ assem = "CALL "^n^"\n", (* page 204. CHECK. *)
                                                 src = munchArgs args frame,
                                                 dst = callersaves @ argregs,
                                                 jump = NONE})
              | EXP (TEMP t) => () (* TODO: que significa esto? *)
              | EXP _ => () (* TODO: CHECK *)
              | JUMP (NAME n, l) => emit (OPER{ assem = "J `j0\n",
                                                 src = [],
                                                 dst = [],
                                                 jump = SOME l})
              | CJUMP(oper,e1,e2,t,f) => ( emit (OPER{ assem = (get_relop_assem oper)^", `j0\n",
                                                 src = [munchExp e1, munchExp e2],
                                                 dst = [],
                                                 jump = SOME [t,f]})) (* por canon este cjump tiene a continuacion el label f *)
              | _ => emit(OPER{assem = "MISSING\n", 
                        src = [], 
                        dst = [], 
                        jump = NONE})

      and safeMunchExp e =
        case e of
          (BINOP (b, e1, e2)) =>
            result (fn r => emit(OPER{assem=(get_binop_code b false)^" `d0, `s0, `s1\n", 
                                      dst=[r],
                                      src=[munchExp e1, munchExp e2], 
                                      jump=NONE}))
          | (MEM e) =>
            result (fn r => emit(OPER{assem="LD `d0, 0(`s0)\n",
                                      dst=[r], 
                                      src=[munchExp e], 
                                      jump=NONE}))
          | _ => raise Fail ("[safeMunchExp] unknown thing")

      (* munchEXP and muncStm will produce tigerassem.Assem instructions as side effects *)
      (* munchExp : tigertree.exp -> tigertree.temp; returns the temporary in which the result is held *)
      and munchExp e =  (*p. 204 205*)   
        case e of
          (TEMP t) => t
          | (CONST 0) => zero
          | (CONST i) =>
            result (fn r => emit(OPER{assem="LI `d0, "^ppint i^"\n",
                                      dst=[r], 
                                      src=[], 
                                      jump=NONE}))
          | (BINOP (b, TEMP "sp", CONST i)) =>
            result (fn r => emit(OPER{assem=(get_binop_code b true)^" `d0, "^sp^", "^ppint i^"\n", 
                                            dst=[r], 
                                            src=[], 
                                            jump=NONE}))
          | (BINOP (b, TEMP "s0", CONST i)) =>
            result (fn r => emit(OPER{assem=(get_binop_code b true)^" `d0, "^fp^", "^ppint i^"\n", 
                                            dst=[r], 
                                            src=[], 
                                            jump=NONE}))
          | (BINOP(MINUS, e1, CONST i)) =>
            munchExp (BINOP(PLUS, e1, CONST (~i)))
          | (BINOP(b, CONST i, e1)) =>
            if valid_imm i andalso binop_supports_imm b andalso binop_commutes b then
            result (fn r => emit(OPER{assem=(get_binop_code b true)^" `d0, `s0, "^ppint i^"\n", 
                                            dst=[r], 
                                            src=[munchExp e1], 
                                            jump=NONE}))
            else safeMunchExp e
          | (BINOP (b, e1, CONST i)) =>
            if valid_imm i andalso binop_supports_imm b then
            result (fn r => emit(OPER{assem=(get_binop_code b true)^" `d0, `s0, "^ppint i^"\n", 
                                      dst=[r], 
                                      src=[munchExp e1], 
                                      jump=NONE}))
            else safeMunchExp e
          | (BINOP _) => safeMunchExp e

          (*Para acceder a memoria tenemos que usar "LW rd,rs1,imm"  <=> rd <- mem[rs1 + imm]*)
          | (MEM (CONST i)) =>
            if valid_imm i then
            result (fn r => emit(OPER{assem="LD `d0, "^ppint i^"(x0)\n",
                                      dst=[r], 
                                      src=[], 
                                      jump=NONE}))
            else safeMunchExp e
          | (MEM (BINOP (PLUS, e1, CONST i))) =>
            if valid_imm i then
            result (fn r => emit(OPER{assem="LD `d0, "^ppint i^"(`s0)\n",
                                      dst=[r], 
                                      src=[munchExp e1], 
                                      jump=NONE}))
            else safeMunchExp e
          | (MEM (BINOP (PLUS, CONST i, e2))) =>
            if valid_imm i then
            result (fn r => emit(OPER{assem="LD `d0, "^ppint i^"(`s0)\n",
                                      dst=[r], 
                                      src=[munchExp e2], 
                                      jump=NONE}))
            else safeMunchExp e
          | (MEM _) => safeMunchExp e
          | (NAME n) => 
            result (fn r => emit(OPER{assem="LA `d0, "^n^"\n",
                                      dst=[r], 
                                      src=[], 
                                      jump=NONE}))
          | (CALL _) =>  raise Fail ("[munchExp] There should not be CALL nodes outside EXP (canonized tree)") 
          | (ESEQ _ ) =>  raise Fail ("[munchExp] There should not be ESEQ nodes anymore (canonized tree)") 
                
          

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

        and munchArgs args frame = let
          (* Destinations on registers *)
          val argregs = List.map TEMP tigerframe.argregs
          (* Destinations on stack *)
          val stackregqty = Int.max(0, (List.length args) - (List.length argregs))
          val _ = tigerframe.allocCallArgs frame stackregqty
          fun argpos x = MEM(BINOP(PLUS, TEMP tigerframe.sp, CONST (x * tigerframe.wSz)))
          val stackpos = List.tabulate(stackregqty, argpos)
          (* now move args -> [regs, stack] *)
          val expreg = ListPair.zip (argregs @ stackpos, args)
          val _ = List.app (munchStm o tigertree.MOVE) expreg
          (* now calculate effectively used regs to return for CALL to use *)
          val regsusedqty = Int.min(List.length tigerframe.argregs, List.length args)
        in List.take (tigerframe.argregs, regsusedqty) end
            
    in
        munchStm stm ; rev(!ilist)
    end

end
