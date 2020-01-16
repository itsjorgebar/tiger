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

(* Activity 1.1 *)

fun maxargs(s : stm) : int =
   case s of
      CompoundStm(a, b) => Int.max(maxargs(a), maxargs(b))
   |  AssignStm(a, b) => maxargsExp(b)
   |  PrintStm(l as x::xs) => Int.max(Int.max(length(l), maxargsExp(x)), maxargs(PrintStm(xs)))
   |  PrintStm(nil) => 0

 and maxargsExp(e : exp) : int =
   case e of
      IdExp(_) => 0
   |  NumExp(_) => 0
   |  OpExp(a, _, c) => Int.max(maxargsExp(a), maxargsExp(c))
   |  EseqExp(a, b) => Int.max(maxargs(a), maxargsExp(b));

 maxargs(prog);

(* Activity 1.2 *)

type table = (id * int) list;

fun update(a:string, b:int, T) = (a, b)::T;

fun lookup(t:table as (a, b)::xs, c:id) = if a=c then b else lookup(xs, c)
 |  lookup(_, c) = 0;

fun operate(a:int, opt:binop, b:int) : int =
    case opt of
      Plus => a + b
   |  Minus => a - b
   |  Times => a * b
   |  Div => a div b;

fun interpStm(s : stm, t : table) : table =
    case s of
      CompoundStm(a, b) => interpStm(b, interpStm(a, t))
   |  AssignStm(a, b) =>
         let
            val (i1, t1) = interpExp(b, t)
         in
            update(a, i1, t1)
         end
   |  PrintStm(x::xs) =>
         let
            val (i1, t1) = interpExp(x, t)
         in
            print(Int.toString(i1)^" "); interpStm(PrintStm(xs), t1)
         end
   |  PrintStm(nil) => (print("\n"); t)

and interpExp(e : exp, t : table) : (int * table) =
     case e of
      NumExp(a) => (a, t)
   |  IdExp(a) => (lookup(t, a), t)
   |  OpExp(a, b, c) =>
         let
            val (i1, t1) = interpExp(a, t)
            val (i2, t2) = interpExp(c, t1)
         in
            (operate(i1, b, i2), t2)
         end
   |  EseqExp(a, b) => interpExp(b, interpStm(a, t));

fun interp(a : stm) : unit = (interpStm(a, []); ());

interp(prog)