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
    |   OpExp(exp1,_,exp2)::t => Int.max(maxargs_exp([exp1,exp2]), maxargs_exp(t))
    |   EseqExp(stm,exp)::t => Int.max(maxargs_exp t, Int.max(maxargs stm, maxargs_exp [exp]));

(* Interpreter*)
type table = (id * int) list

fun update(id : id, v : int, tbl : table) : table = (id,v)::tbl;

fun lookup(tbl : table, id : id) : int = 
    case tbl of
        [] => 0 (* invalid program *)
    |   (idM,vM)::t => if id = idM then vM else lookup(t,id);

fun evalop(a : int, opt : binop, b : int) : int =
    case opt of
        Plus => a + b
    |   Minus => a - b
    |   Times => a * b
    |   Div => a div b;

fun interp(s : stm) : unit =
    case s of
        CompoundStm(s1,s2) => (interpStm(s2, interpStm(s1, [])); ())
    |   PrintStm([]) => ()
    |   PrintStm(h::t) => (interpStm ((PrintStm t), (interpStm (PrintStm [h], []))); ())
    |   AssignStm(id,exp) => ()
and interpStm(s : stm, tbl : table) : table =
    case s of
        CompoundStm(s1,s2) => interpStm(s2, interpStm(s1, tbl))
    |   AssignStm(id,exp) => 
            let 
                val (vIn, tblIn) = interpExp(exp, tbl)
            in
                update(id, vIn, tblIn)
            end
    |   PrintStm([]) => (print "\n"; tbl)
    |   PrintStm(h::t) => 
            let
              val (num, tblIn) = interpExp (h, tbl)
            in
                (print (Int.toString num ^ " "); interpStm (PrintStm t, tblIn))
            end
and interpExp(e : exp, tbl : table) : int * table =
    case e of
        IdExp(x) => (lookup (tbl,x),tbl)
    |   NumExp(x) => (x,tbl)
    |   OpExp(aP,opt,bP) => 
            let
              val (a,tblA) = interpExp(aP,tbl)
              val (b,tblB) = interpExp(bP,tblA)
            in
              (evalop(a,opt,b), tblB)
            end
    |   EseqExp(stm,ex) => interpExp(ex, interpStm(stm, tbl));