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
                | TTipo of string 

end
