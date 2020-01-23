structure Tokens : Tiger_TOKENS =
struct
  (* A "scaffold" structure for debugging lexers. *)

type linenum = int
type token = string
fun R1(c,i,j) = "R1("^c^")   " ^ Int.toString(i)
fun R2(c,i,j) = "R2("^c^")   " ^ Int.toString(i)
fun R3(c,i,j) = "R3("^c^")   " ^ Int.toString(i)
fun EOF(i,j) = "EOF   " ^ Int.toString(i)
end
