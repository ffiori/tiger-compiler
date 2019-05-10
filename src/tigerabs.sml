structure tigerabs = 
struct

type symbol = string
type pos = int

datatype var = SimpleVar of symbol
	| FieldVar of var * symbol
	| SubscriptVar of var * exp

and exp = VarExp of var * pos
	| UnitExp of pos
	| NilExp of pos
	| IntExp of int * pos
	| StringExp of string * pos
	| CallExp of {func: symbol, args: exp list} * pos
	| OpExp of {left: exp, oper: oper, right: exp} * pos
	| RecordExp of {fields: (symbol * exp) list, typ: symbol} * pos
	| SeqExp of exp list * pos
	| AssignExp of {var: var, exp: exp} * pos
	| IfExp of {test: exp, then': exp, else': exp option} * pos (* option = maybe de haskell *)
	| WhileExp of {test: exp, body: exp} * pos
	| ForExp of {var: symbol, escape: bool ref, lo: exp, hi: exp, body: exp} * pos (* for a:=10 to 20 do ... *) (* escape indica si después se va a usar en alguna función interior la variable que se usa como contador, por ejemplo en el cuerpo del for: let function f(b:int)=a+b in printint(f(7)) *) (* es bool ref porque en una primera etapa el parser no puede definir si es escapada o no, debería ir y venir mucho en el código y es muy complicado. Se deja sin definir hasta una segunda recorrida del AST- *)
	| LetExp of {decs: dec list, body: exp} * pos
	| BreakExp of pos
	| ArrayExp of {typ: symbol, size: exp, init: exp} * pos (* type A=array of int
                                                                   var b:=A[10] of 7 *)

and dec = FunctionDec of ({name: symbol, params: field list, result: symbol option, body: exp} * pos) list (* batch de declaraciones de funciones *)
        | VarDec of {name: symbol,
                     escape: bool ref,
                     typ: symbol option, (* viene en none si no se tipó estáticamente la expresión. Sería como el auto en c++. *)
                     init: exp} * pos
        | TypeDec of ({name: symbol, ty: ty} * pos) list

and ty = NameTy of symbol (* en symbol va el nombre del tipo al que referencia el tipo nuevo *)
    | RecordTy of field list
	| ArrayTy of symbol

and oper = PlusOp | MinusOp | TimesOp | DivideOp
	| EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp

withtype field = {name: symbol, escape: bool ref, typ: ty}
end
