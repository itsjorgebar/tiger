structure A = Absyn

structure Translate = struct type exp = unit end

signature SEMANT =
sig
  type venv = Env.enventry Symbol.table
  type tenv = Types.ty Symbol.table
  type expty = {exp: Translate.exp, ty: Types.ty}

  val transVar: venv * tenv * Absyn.var -> expty
  val transExp: venv * tenv -> Absyn.exp -> expty
  val transDec: venv * tenv * Absyn.dec -> {venv: venv, tenv: tenv}
  val transTy:         tenv * Absyn.ty -> Types.ty
  val transProg: Absyn.exp -> unit
end

structure Semant : SEMANT =
struct
  type venv = Env.enventry Symbol.table
  type tenv = Types.ty Symbol.table
  type expty = {exp: Translate.exp, ty: Types.ty}

  fun prettyPrint exp = PrintAbsyn.print(TextIO.stdOut, exp)
  fun transVar(venv, tenv, var) = {exp=(), ty=Types.NIL}

  fun transExp(venv, tenv, exp) = {exp=(), ty=Types.NIL}
  fun checkint ({exp,ty},pos) = ()
  fun transExp(venv,tenv) =
    let fun trexp (A.OpExp{left,oper=A.PlusOp,right,pos}) =
                  (checkint(trexp left, pos);
                  checkint(trexp right, pos);
                  {exp=(),ty=Types.INT})
    in trexp
    end
  fun transDec(venv, tenv, dec) = {venv=venv, tenv=tenv}
  fun transTy(tenv: tenv, ty: Absyn.ty) = Types.NIL
  fun transProg exp = prettyPrint exp
end