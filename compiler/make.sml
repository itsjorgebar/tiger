print "Compiling sources.cm\n";
CM.make "sources.cm";
print "\n\n";

print "Parsing test.tig\n";
Parse.parse "test.tig";
print "\n\n";

print "Pretty print\n";
PrintAbsyn.print(TextIO.stdOut, Parse.parse("test.tig"));
print "\n\n";