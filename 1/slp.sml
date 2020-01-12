(* Book code *)
type id = string

datatype binop = Plus | Minus | Times | Div

datatype stm = CompoundStm of stm * stm
	     | AssignStm of id * exp
	     | PrintStm of exp list

     and exp = IdExp of id
	     | NumExp of int
             | OpExp of exp * binop * exp
             | EseqExp of stm * exp
val prog = 
 CompoundStm(AssignStm("a",OpExp(NumExp 5, Plus, NumExp 3)),
  CompoundStm(AssignStm("b",
      EseqExp(PrintStm[IdExp"a",OpExp(IdExp"a", Minus,NumExp 1)],
           OpExp(NumExp 10, Times, IdExp"a"))),
   PrintStm[IdExp "b"]))

(* My code *)

(* Calculates maximum arguments in any print statement of a statement *)
fun maxargs(s:stm) : int =
    case s of 
        AssignStm(_,exp) => maxargs_exp([exp])
    |   PrintStm(x) => Int.max(length x, maxargs_exp x)
    |   CompoundStm(stm1, stm2) => Int.max(maxargs stm1, maxargs stm2)
and maxargs_exp(el: exp list) : int =
    case el of
        [] => 0
    |   IdExp(_)::t => maxargs_exp(t)
    |   NumExp(_)::t => maxargs_exp(t)
    |   OpExp(exp1,_,exp2)::t => maxargs_exp([exp1,exp2])
    |   EseqExp(stm,exp)::t => Int.max(maxargs_exp t, Int.max(maxargs stm, maxargs_exp [exp]));