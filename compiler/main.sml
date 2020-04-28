structure Main: sig val run : string -> Translate.frag list end =
struct

    fun run filename = 
      let val ast = Parse.parse filename
          fun prettyPrint exp = PrintAbsyn.print(TextIO.stdOut, exp)
      in Semant.transProg ast
          (*prettyPrint ast*)
      end

end