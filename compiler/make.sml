print "Compiling sources.cm\n";
CM.make "sources.cm";
print "\n\n";

let
    val filename = "examples/mutrec.tig"
in
    print ("Parsing " ^ filename ^ "\n");
    Parse.parse filename;
    print "\n\n";

    print ("Pretty print of " ^ filename ^ "\n");
    PrintAbsyn.print(TextIO.stdOut, Parse.parse(filename));
    print "\n\n"
end