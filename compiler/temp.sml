signature TEMP =
sig
    eqtype temp
    val newtemp : unit -> temp
    (*
    structure Table : TABLE sharing type Table.key = temp
    val makestring: temp -> string
    val namedlabel : string -> label
    *)
    type label = Symbol.symbol
    val newlabel : unit -> label
end 

(*TODO correct all of this*)
structure Temp : TEMP = 
struct 
    type temp = int
    type label = Symbol.symbol
    fun newlabel() = Symbol.symbol "dummy"
    fun newtemp() = 0;
end