structure tigertree =
struct
	datatype exp = CONST of int  (* Constante entera *)
		     | NAME of tigertemp.label (* hace referencia a un LABEL *)
		     | TEMP of tigertemp.temp (* Como un registro, pero hay infinitos *)
		     | BINOP of binop*exp*exp (*  +-*/ AND OR XOR LSHIFT RSHIFT ARSHIFT*)
		     | MEM of exp (* Wordsize bytes starting at address exp *)
		     | CALL of exp*exp list (* f( ... ) *)
		     | ESEQ of stm*exp (* ejecuta stm, y después exp y devuelve lo que devuelve exp *)
	and stm = MOVE of exp*exp (* MOVE(temp e1,e2) o MOVE(mem(e1),e2)  <-*)
		| EXP of exp (*evalua la expresion y la descarta *)
		| JUMP of exp*tigertemp.label list (* JUMP (NAME l,[l]) siempre va a ser así, saltar a label l *)
		| CJUMP of relop*exp*exp*tigertemp.label*tigertemp.label (* relop(e1,e2) ? jump l1 : jump l2 *)
		| SEQ of stm*stm (* ejecuta el primer stm y después el segundo *)
		| LABEL of tigertemp.label (* hace label: *) 
	and binop = PLUS | MINUS | MUL | DIV | AND | OR
		  | LSHIFT | RSHIFT | ARSHIFT | XOR
	and relop = EQ | NE | LT | GT | LE | GE | ULT | ULE
		  | UGT | UGE

	fun notRel EQ = NE
	  | notRel NE = EQ
	  | notRel LT = GE
	  | notRel GE = LT
	  | notRel GT = LE
	  | notRel LE = GT
	  | notRel ULT = UGE
	  | notRel UGE = ULT
	  | notRel ULE = UGT
	  | notRel UGT = ULE
end
