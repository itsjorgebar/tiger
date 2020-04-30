structure Main: sig val run : string -> unit end =
struct

    fun run filename = 
      let val ast = Parse.parse filename
          fun prettyPrint exp = PrintAbsyn.print(TextIO.stdOut, exp)
      in Semant.printIR ast
      (*Semant.transProg ast*)
          (*prettyPrint ast*)
      end

end