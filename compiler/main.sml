structure Main: sig val run : string -> Tr.frag list end =
struct

    fun run filename = 
      let 
          val ast = Parse.parse filename
          fun printAST exp = PrintAbsyn.print(TextIO.stdOut, exp)
          val (ir,frags) = Semant.expAndFrags ast
          fun printIR exp = Tr.printIR exp
          
      in (Utils.print "\n***AST***\n"; printAST ast; Utils.print "\n";
          Utils.print "***IR***\n"; printIR ir; Utils.print "\n"; frags)
      end

end