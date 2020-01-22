signature Tiger_TOKENS =
sig
type linenum (* = int *)
type token
val R1: (int) *  linenum * linenum -> token
val R2: (int) *  linenum * linenum -> token
val R3: (int) *  linenum * linenum -> token
val EOF:  linenum * linenum -> token
end
