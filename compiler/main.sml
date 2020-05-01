structure Main: sig val run : string -> Tr.frag list end =
struct

    fun run filename = 
      let fun print s = TextIO.output(TextIO.stdOut,s)

          val ast = Parse.parse filename
          fun printAST exp = PrintAbsyn.print(TextIO.stdOut, exp)

          val (ir,frags) = Semant.expAndFrags ast
          fun printIR exp = Tr.printIR exp
          
      in (print "\n***AST***\n"; printAST ast; print "\n";
          print "***IR***\n"; printIR ir; print "\n"; frags)
      end

end