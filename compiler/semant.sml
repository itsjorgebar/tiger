structure A = Absyn
structure S = Symbol
structure T = Types

structure Translate = struct type exp = unit end

signature SEMANT =
sig
  type venv = Env.enventry Symbol.table
  type tenv = T.ty Symbol.table
  type expty = {exp: Translate.exp, ty: T.ty}

  (* Recursively type-checks an AST *)
  val transProg: Absyn.exp -> unit
  val transVar: venv * tenv * Absyn.var -> expty
  val transExp: venv * tenv -> Absyn.exp -> expty
  val transDec: venv * tenv * Absyn.dec -> {venv: venv, tenv: tenv}
  val transTy:         tenv * Absyn.ty -> T.ty
end

structure Semant : SEMANT =
struct
  type venv = Env.enventry Symbol.table
  type tenv = T.ty Symbol.table
  type expty = {exp: Translate.exp, ty: T.ty}

  datatype aritmoper = A.PlusOp | A.MinusOp | A.TimesOp | A.DivideOp
  and eqoper = A.EqOp | A.NeqOp
  and compoper = A.LtOp | A.LeOp | A.GtOp | A.GeOp

  fun prettyPrint exp = PrintAbsyn.print(TextIO.stdOut, exp)
  fun transVar(venv, tenv, var) = {exp=(), ty=T.NIL}

  fun checkint ({exp, ty}, pos) = 
    case ty of 
      T.INT => ()
    | _ => ErrorMsg.error pos "integer required"

  fun checkeqty ({exp1, ty1}, {exp2, ty2}, pos) =
    if
      ty1 = ty2
    then
      ty1
    else ErrorMsg.error pos "Operand types do not match"
  
  fun transExp(venv,tenv) =
    let
      fun trexp e = 
        case e of
          A.VarExp(var) => {exp=(), trvar var}
        | A.NilExp => 
            {exp=(), ty=T.NIL}
        |  A.IntExp(_) => 
            {exp=(), ty=T.INT}
        | A.StringExp(_,_) => 
            {exp=(), ty=T.STRING}
        | A.CallExp{func,args,pos} => 
          let val (name, formals, result) = S.look(venv,func)
              fun comp(arg,formal) = if #ty (trexp arg) = formal  (*TODO fix equality*)
                                     then ()
                                     else ErrorMsg.error pos 
                                          ("Type mismatch between function formal parameters and arguments")
          in (app comp zipEq(args,formals) 
              handle UnequalLengths => 
                ErrorMsg.error pos ("Invalid number of arguments in function call");
                {exp=(),ty=result})
          end
        | A.OpExp{left,oper=aritmoper,right,pos} =>
            (checkint(trexp left, pos);
            checkint(trexp right, pos);
            {exp=(), ty=T.INT})
        | A.OpExp{left,oper=eqoper,right,pos} =>
            {exp=(), ty=checkeqty(trexp left, trexp right, pos)}
        | A.OpExp{left,oper=compoper,right,pos} =>
            let
              val ty = checkeqty(trexp left, trexp right, pos)
            in
              if
                ty = T.STRING
              then
                ErrorMsg.error pos ("Cannot use relational operators on strings")
              else
                {exp=(), ty=ty}
            end
        | A.SeqExp((exp,_)::[]) => trexp exp
        | A.SeqExp((exp,_)::exps) => (trexp exp; transExp(venv', tenv') A.SeqExp(exps))
        | A.LetExp{decs,body,pos} =>
            let val {venv=venv',tenv=tenv'} =
                  transDec(venv,tenv,decs)
            in transExp(venv',tenv') body
            end
        | A.RecordExp{fields=[],typ,pos} =>
            {exp=(), ty=typ}
        | A.RecordExp{fields=(symbol, exp, recpos)::xs,typ,pos} =>
            checkeqty(Symbol.look(venv, symbol), trexp exp, recpos);
            trexp A.RecordExp{fields=xs, typ=typ, pos=pos}
        | A.AssignExp{var=A.SimpleVar(symbol, varpos),exp,pos} =>
            {exp=(), ty=checkeqty(Symbol.look(venv, symbol), trexp exp, varpos)}
        | A.AssignExp{var=A.FieldVar(var, symbol, varpos),exp,pos} => () (* TODO complete*)
        | A.AssignExp{var=A.SubscriptVar(var, exp, varpos),exp,pos} => () (* TODO complete*)
        | A.IfExp{test, then', else', pos} =>
            (checkInt trexp(test); {exp=(), ty=checkeqty(trexp then', trexp else', pos)})
        | A.WhileExp{test,body,pos} =>
            (checkInt trexp(test); {exp=(), ty=checkeqty(trexp body, T.UNIT, pos)})
        | A.BreakExp(pos) => {exp=(), ty=T.UNIT}
        | A.ArrayExp{typ,size,init,pos} =>
            {exp=(), ty=checkeqty(trexp init', typ, pos)}
        | A.ForExp{var=A.SimpleVar(symbol, varpos),escape,lo,hi,body,pos} =>
            let
              val vd = VarDec{name=symbol, escape=ref true, typ=NONE, init=lo, pos=varpos}
              val {venv=venv', tenv=tenv'} = transDec(venv, tenv, vd)
            in
              (checkInt(trexp lo);
              checkInt(trexp hi);
              {exp=(), ty=checkeqty (transExp(venv', tenv') body, { exp=(), ty=T.UNIT }, pos)})
            end
        | A.ForExp{var,escape,lo,hi,body,pos} => (*TODO complete*)
        | _ =>
            {exp=(), ty=T.UNIT} (*TODO: change ty*)
      and trvar e =
        case e of
          A.SimpleVar(id,pos) =>
            (case 
              Symbol.look(venv,id) of SOME(E.VarEntry{ty}) => 
                {exp=(), ty=actual_ty ty}
            | NONE => (ErrorMsg.error pos ("undefined variable " S.name id);
                {exp=(), ty=T.INT}))
        | A.FieldVar(inVar,sym,pos) =>
            let
              val {ty} = trvar inVar
              fun fl_look([], target) = 
                    ErrorMsg.error pos ("Record field " ^ S.name target ^" type not defined")
                  fl_look((currSym,ty)::xs, target) = 
                    if currSym = target 
                    then {exp=(), ty=y}
                    else recLook(xs,target)
            in
              case ty of 
                T.RECORD(fl,_) => fl_look(fl, sym)
              | _ => ErrorMsg.error pos ("Field dot suffix cannot be applied to a non-record type variable")
            end
        | A.SubscriptVar(inVar,exp,pos) =>
            let
              val {ty} = trvar inVar
            in
              case ty of 
                T.ARRAY(arrTy,_) => (checkint (trexp exp); arrTy)
              | _ => ErrorMsg.error pos ("Subscript suffix cannot be applied to a non-array type variable")
            end
    in trexp
    end

  fun transDec(venv, tenv, dec) = 
    case dec of 
      A.VarDec{name, escape, typ=NONE, init, pos} =>
        let val {exp,ty} = transExp(venv,tenv) init
        in {tenv=tenv, venv=S.enter(venv,name,E.VarEntry{ty=ty})}
        end
    | A.VarDec{name, escape, typ=SOME(sym,_), init, pos} =>
        let val {exp,ty} = transExp(venv,tenv) init
            val expected = tenv.look sym
        in 
          if expected = ty 
          then {tenv=tenv, venv=S.enter(venv,name,E.VarEntry{ty=ty})} 
          else  ErrorMsg.error pos ("Variable type does not match expression result"); {exp=(), ty=T.UNIT}
        end
    | A.TypeDec[] => {venv=venv, tenv=tenv}
    | A.TypeDec[{name,ty,pos}::t] => 
        let val {venv', tenv'} = {venv=venv,tenv=S.enter(tenv,name,transTy(tenv,ty))}
        in transDec(venv', tenv', A.TypeDec t)
        end
    | A.FunctionDec[{name,params,body,pos,result}]) =
        let 
          fun type_error typ = ("Type" ^ S.name typ ^ " is not defined.")
          val result_ty = 
            case result of 
              NONE => T.UNIT
            | SOME(rt,pos) =>
                case S.look(tenv,rt) of
                  SOME(result_ty') => result_ty'
                | NONE =>  ErrorMsg.error pos type_error(rt)
          fun transparam{name,typ,pos} =
            case S.look(tenv,typ) of 
              SOME t => {name=name,ty=t}
            | NONE => ErrorMsg.error pos type_error(typ); {name="", ty=T.UNIT}
          val params' = map transparam params
          val venv' = S.enter(venv,name, E.FunEntry{formals= map #ty params',
                                                    result= result_ty})
          fun enterparam ({name,ty},venv) = 
            S.enter(venv,name, E.VarEntry{access=(),ty=ty})
          val venv'' = fold enterparam params' venv' (* TODO maybe specify foldr *)
        in 
          let
            {ty=body_ty} = transExp(venv'',tev) body
          in
            if
              body_ty = result_ty
            then
              {venv=venv',tenv=tenv}
            else
              ErrorMsg.error pos ("Function return type does not match functionn declaratio");
              {venv=venv',tenv=tenv}
          end
        end

  fun transTy tenv = 
      let fun trty ty = case ty of 
            A.NameTy(symbol,pos) => NAME(symbol, S.look(tenv,symbol)) (*We don't know if (*FIX we do not understand NameTy??*)*)
          | A.RecordTy(fl) => 
              let fun trty' [] = []
                    | trty' {name, typ, pos}::xs =
                        let
                          val ty = case S.look(tenv,typ) of
                                      NONE => ErrorMsg.error pos ("Data type not declared in this scope");
                                    | SOME(ty') => ty'
                        in
                          (name, ty)::trty'(xs)
                        end
              in T.RECORD{trty' fl, ref ()}
              end
          | A.ArrayTy(symbol,pos) =>
              let val ty = case S.look(tenv,symbol) of
                              NONE => ErrorMsg.error pos ("Data type not declared in this scope");
                            | SOME(ty') => ty'
              in T.ARRAY(ty, ref ())
              end
      in trty
      end

  fun transProg exp = (transExp(Env.base_venv, Env.base_tenv) exp; ())
end