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

  fun prettyPrint exp = PrintAbsyn.print(TextIO.stdOut, exp)
  fun transVar(venv, tenv, var) = {exp=(), ty=T.NIL}

  fun checkint ({exp, ty}, pos) = 
    case ty of 
      T.INT => ()
    | _ => ErrorMsg.error pos "integer required"

  fun transExp(venv,tenv) =
    let 
      fun trexp e = 
        case e of
          A.IntExp(num) => 
            {exp=(), ty=T.INT}
        | A.StringExp(num,_) => 
            {exp=(), ty=T.STRING}
        | A.NilExp => 
            {exp=(), ty=T.NIL}
        | A.OpExp{left,oper=_,right,pos} =>
            (checkint(trexp left, pos);
            checkint(trexp right, pos);
            {exp=(), ty=T.INT})
        | A.SeqExp((exp,_)::[]) => trexp exp 
        | A.SeqExp((exp,_)::exps) => (trexp exp; trexp(A.SeqExp(exps)))
        | A.LetExp{decs,body,pos} =
            let val {venv=venv',tenv=tenv'} =
              transDec(venv,tenv,decs)
            in transExp(venv',tenv') body
            end
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
          | A.FieldVar(v,id,pos)) = 
            trvar v (*TODO complete*)
    in trexp
    end

  fun transDec(venv, tenv, dec) = 
    case dec of 
      A.VarDec{name, escape, typ=NONE, init, pos} =>
        let val {exp,ty} = transExp(venv,tenv,init)
        in {tenv=tenv, venv=S.enter(venv,name,E.VarEntry{ty=ty})}
        end
    | A.VarDec{name, escape, typ=SOME(sym,_), init, pos} =>
        let val {exp,ty} = transExp(venv,tenv,init)
            val expected = tenv.look sym
        in 
          if expected = ty 
          then {tenv=tenv, venv=S.enter(venv,name,E.VarEntry{ty=ty})} 
          else  ErrorMsg.error pos ("Variable type does not match expression result")
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
            | NONE => ErrorMsg.error pos type_error(typ)
          val params' = map transparam params
          val venv' = S.enter(venv,name, E.FunEntry{formals= map #ty params',
                                                    result= result_ty})
          fun enterparam ({name,ty},venv) = 
            S.enter(venv,name, E.VarEntry{access=(),ty=ty})
          val venv'' = fold enterparam params' venv' (* TODO maybe specify foldr *)
        in 
          let {ty=body_ty} = transExp(venv'',tev) body
          in if body_ty = result_ty then {venv=venv',tenv=tenv}
             else ErrorMsg.error pos ("Function return type does not match functionn declaratio")
          end
        end

  fun transTy tenv = 
      let fun trty ty = case ty of 
            A.NameTy(symbol,pos) => NAME(symbol, S.look(tenv,symbol))
          | A.RecordTy(fl) => 
              let fun trty' [] = []
                    | trty' {name, typ, pos}::xs =
                        (name, S.look(tenv,typ))::trty'(xs)
              in T.RECORD{trty' fl, ref ()}
              end
          | A.ArrayTy(symbol,pos) =>
              let val ty = case S.look(tenv,symbol) of
                              NONE => (* error *)
                            | SOME(ty') => ty'
              in T.ARRAY(ty, ref ())
              end
      in trty
      end

  fun transProg exp = (transExp(Env.base_venv, Env.base_tenv) exp; ())
end