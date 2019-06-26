structure tigerassem = struct

    (* Assembly language instruction without register assignments p. 201 *)
    type reg = string
    type temp = tigertemp.temp
    type label = tigertemp.label

    datatype instr =
        OPER of 
            {assem: string, (* assembly language instruction, e.g. "MUL 'd0 <- 's0*'s1" *)
            dst: temp list, (* operand registers *)
            src: temp list, (* result registers *)
            jump: label list option (* jump=NONE : Ops that always fall to the next instruction *)
            }                       (* jump = ls : Ops that have a list of target labes to which
                                       they might jump *)
        | LABEL of { (* LABEL: Point in the program to which jumps may go *)
            assem: string, (* string: how the label will look in assembly lang *)
            lab: tigertemp.label} (* label id *)
        | MOVE of { (* Special case of OPER that only performs data transfer *)
            assem: string, 
            dst: temp,
            src: temp}

	(* Reemplaza el temporal t1 por t2 en src de instr *)
	fun replaceTempSrc instr t1 t2 =
		case instr of
			LABEL _ => instr
			| MOVE {src=src,dst=dst,assem=assem} =>
				MOVE {src=if src=t1 then t2 else src, dst=dst, assem=assem}
			| OPER {src=src,dst=dst,assem=assem,jump=jump} =>
				OPER {src=List.map (fn t=>if t=t1 then t2 else t) src, dst=dst, assem=assem, jump=jump}

	fun replaceTempDst instr t1 t2 =
		case instr of
			LABEL _ => instr
			| MOVE {src=src,dst=dst,assem=assem} =>
				MOVE {dst=if dst=t1 then t2 else dst, src=src, assem=assem}
			| OPER {src=src,dst=dst,assem=assem,jump=jump} =>
				OPER {dst=List.map (fn t=>if t=t1 then t2 else t) dst, src=src, assem=assem, jump=jump}

	fun getsrc (OPER {src=src,...}) = src
	| getsrc (MOVE {src=src,...}) = [src]
	| getsrc _ = []
	
	fun getdst (OPER {dst=dst,...}) = dst
	| getdst (MOVE {dst=dst,...}) = [dst]
	| getdst _ = []
	
    fun compare (MOVE i1, MOVE i2) = 
        if #assem i1 = #assem i2
        then
            if #src i1 = #src i2
            then String.compare(#dst i1, #dst i2)
            else String.compare(#src i1, #src i2)
        else String.compare(#assem i1, #assem i2)
    | compare _ = raise Fail "tigerassem.compare Should not happen, should only be used to compare MOVEs.\n" 

  (* format : (temp -> string) -> instr -> string 
    - format(m)(i) formats an asssembly instriction as a string
    - m is the function that shows the register assignment    *)
    fun format saytemp = 
        let fun speak(assem,dst,src,jump) =
            let fun saylab s = s
                fun f(#"`":: #"s":: i::rest) = 
                        (explode(saytemp(List.nth(src,ord i - ord #"0"))) @ f rest)
                    | f( #"`":: #"d":: i:: rest) = 
                        (explode(saytemp(List.nth(dst,ord i - ord #"0"))) @ f rest)
                    | f( #"`":: #"j":: i:: rest) = 
                        (explode(saylab(List.nth(jump,ord i - ord #"0"))) @ f rest)
                    | f( #"`":: #"`":: rest) = #"`" :: f rest
                    | f( #"`":: _ :: rest) = raise Fail "bad Assem format"
                    | f(c :: rest) = (c :: f rest)
                    | f nil = nil
            in 
                "\t"^implode(f(explode assem))
            end

        in fn OPER{assem="",dst,src,jump} => ""
            | OPER{assem,dst,src,jump=NONE} => speak(assem,dst,src,nil)
            | OPER{assem,dst,src,jump=SOME j} => speak(assem,dst,src,j)
            | LABEL{assem,...} => assem
            | MOVE{assem,dst,src} => speak(assem,[dst],[src],nil)
        end

    (* (string -> unit) -> {prolog,body,epilog} list -> unit *)
    fun mapAssem f (x: {prolog:string,body:instr list,epilog:string} list) = 
        List.app (showPBE f) x
    and showPBE f w =
        (f (#prolog w);
        showB f (#body w);
        f (#epilog w))
    and showB f b =
        List.app (fn w => f(format (fn f => f) w)) b

end
