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
                implode(f(explode assem))
            end

        in fn OPER{assem,dst,src,jump=NONE} => speak(assem,dst,src,nil)
            | OPER{assem,dst,src,jump=SOME j} => speak(assem,dst,src,j)
            | LABEL{assem,...} => assem
            | MOVE{assem,dst,src} => speak(assem,[dst],[src],nil)
        end

    (* {prolog,body,epilog} list -> unit *)
    fun showAssem (x: {prolog:string,body:instr list,epilog:string} list) = 
        List.app showPBE x
    and showPBE w =
        (print(#prolog w);
        showB (#body w);
        print(#epilog w))
    and showB b =
        List.app (fn w => print(format (fn f => f) w)) b

end
