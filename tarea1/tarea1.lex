type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() =
  let
    val pos = hd(!linePos)
  in
      Tokens.EOF(pos,pos)
  end

%%
%%
<INITIAL> \n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL> (" "|"\t")+ => (continue());
<INITIAL> (01)*((01)|1*) => (Tokens.R1(yytext, yypos, yypos+size(yytext)));
<INITIAL> (00*)((0|1)*1) => (Tokens.R2(yytext, yypos, yypos+size(yytext)));
<INITIAL> (01*)((2|3|4|h)*|(01)*|5)* => (Tokens.R3(yytext, yypos, yypos+size(yytext)));
<INITIAL>  . => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());