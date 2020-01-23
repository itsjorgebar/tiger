type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end


%% 
alpha=[A-Za-z];
digit=[0-9];
alphanum=[{alpha}{digit}];
%%
\n	=> (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
var  	=> (Tokens.VAR(yypos,yypos+3));
.       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());