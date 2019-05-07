structure tigertips =
struct

type unique = unit ref
datatype Propiedad = RO | RW
datatype Tipo = TUnit
                | TNil
                | TInt of Propiedad
                | TString
                | TArray of Tipo ref  * unique
                | TRecord of (string * Tipo ref * int) list * unique
                | TTipo of string (* Para sinónimos de tipo *) (* TODO FEFO: en el libro lo define como NAME of Symbol.symbol * ty option ref, pero en la página 116, último párrafo habla de que pueden aparecer NAME type, y dice que se puede tener una función que cambie todos los TTipo a los tipos posta a los que refieren. *)

end
