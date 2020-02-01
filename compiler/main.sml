structure Main: sig val run : string -> unit end =
struct

    fun run filename = 
    let
      val ast = Parse.parse filename
    in
        Semant.transProg ast
    end

end