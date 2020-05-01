structure Main: sig val run : string -> unit end =
struct

    fun run filename = 
      let fun print s = TextIO.output(TextIO.stdOut,s)
          val ast = Parse.parse filename
          fun prettyPrint exp = PrintAbsyn.print(TextIO.stdOut, exp)
      in (print "\n***AST***\n"; prettyPrint ast; print "\n";
          print "***IR***\n"; Semant.printIR ast; print "\n";
          (*Semant.transProg ast;*)
          ())
      (*Semant.transProg ast*)
          (*prettyPrint ast*)
      end

end