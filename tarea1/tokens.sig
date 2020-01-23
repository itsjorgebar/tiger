signature Tiger_TOKENS =
sig
type linenum (* = int *)
type token
val R1: (string) *  linenum * linenum -> token
val R2: (string) *  linenum * linenum -> token
val R3: (string) *  linenum * linenum -> token
val EOF:  linenum * linenum -> token
end
