structure A = Absyn
structure S = Symbol
structure E = Env
structure Tr = Translate

signature SEMANT =
sig
  type venv = Env.enventry Symbol.table
  type tenv = T.ty Symbol.table
  (*TODO replace int with a label*)
  type break = int
  type expty = {exp: Tr.exp, ty: T.ty}

  val transExp: venv * tenv * break * Tr.level -> Absyn.exp -> expty
  val transDec: venv * tenv * Absyn.dec * break * Tr.level -> 
                {venv: venv, tenv: tenv, exps: Tr.exp list}
  val transTy: tenv -> Absyn.ty -> T.ty

  (* Recursively type-checks an AST *)
  val transProg: Absyn.exp -> Tr.frag list
end

structure Semant : SEMANT =
struct
  type venv = Env.enventry Symbol.table
  type tenv = T.ty Symbol.table
  type break = int
  type expty = {exp: Tr.exp, ty: T.ty}

  fun prettyPrint exp = PrintAbsyn.print(TextIO.stdOut, exp)
  fun nonoptV ((NONE,pos,lev) | (SOME(E.FunEntry _),pos,lev)) = 
      (ErrorMsg.error pos ("Variable not declared in this scope") ; 
       E.VarEntry{access=Tr.allocLocal lev true, ty=T.UNIT})
  |   nonoptV (SOME entry,_,_) = entry
  fun nonoptT (SOME ty,_) = ty
    | nonoptT (NONE,pos) = (ErrorMsg.error pos 
                            ("Data type not declared in this scope")
                             ; T.UNIT)
  fun checkint ({exp, ty}, pos) = 
    case ty of 
      T.INT => ()
    | _ => ErrorMsg.error pos "integer required"
  fun checkeqty ({ty=ty1,...}:expty, {ty=ty2,...}:expty, pos) =
    if ty1 = ty2 then ty1
    else (ErrorMsg.error pos ("Operand types do not match (" ^ T.tyToStr ty1 ^ 
          "," ^ T.tyToStr ty2 ^ ")."); T.UNIT)
  fun invalidComp pos = (ErrorMsg.error pos ("Invalid comparison types.");
    {exp=Tr.dummy(),ty=T.UNIT})
  fun compData(a,b,ty,pos) = if a = b then {exp=Tr.dummy(), ty=ty} else invalidComp pos
  fun actualTy ty = case ty of T.NAME(_,ref(SOME ty')) => ty' | _ => ty
  fun transTy tenv = 
    let fun trty ty = case ty of 
           A.NameTy(sym,pos) => nonoptT(S.look(tenv,sym),pos)
        |  A.RecordTy(fl) => 
            let fun trty' [] = []
                  | trty' ({name, escape, typ, pos}::xs) =
                      let val ty = nonoptT(S.look(tenv,typ), pos)
                      in (name, ty)::(trty' xs)
                      end
            in T.RECORD (trty' fl, ref ())
            end
        | A.ArrayTy(symbol,pos) =>
            let val ty = nonoptT(S.look(tenv,symbol),pos)
            in T.ARRAY(ty, ref ())
            end
    in trty
    end
  fun type_error typ = ("Type" ^ S.name typ ^ " is not defined.")
  fun transParams (params,tenv) = 
    let fun transParam{name,escape,typ,pos} =
            case S.look(tenv,typ) of 
              SOME t => {name=name,ty=t}
            | NONE => (ErrorMsg.error pos (type_error typ); 
                       {name=name, ty=T.UNIT})
    in map transParam params
    end
  fun transResult(result,tenv) = 
    case result of 
      NONE => T.UNIT
    | SOME(rt,pos) =>
       case S.look(tenv,rt) of
         SOME(result_ty') => result_ty'
       | NONE =>  (ErrorMsg.error pos (type_error rt); T.UNIT)
  fun processMutrecTypeHeaders(tenv,dec) =
      case dec of 
        A.TypeDec[] => tenv
      | A.TypeDec({name,ty,pos}::ds) => 
          let val tenv' = S.enter(tenv,name,T.NAME(name,ref NONE))
          in processMutrecTypeHeaders(tenv',A.TypeDec ds)
          end
      | _ => tenv
  fun processMutrecTypeBodys(tenv,dec,passedRecOrArr,pos') =
      case dec of 
        A.TypeDec[] => if passedRecOrArr then tenv else 
          (ErrorMsg.error pos' "Circular type definitions."; tenv)
      | A.TypeDec({name,ty,pos}::ds) => 
          let val ty' = transTy tenv ty
              val passedRecOrArr' = case ty' of 
                  (T.RECORD _ | T.ARRAY _) => true
                | _ => passedRecOrArr
          in
            (case S.look(tenv,name) of 
              SOME(T.NAME(_,p)) => 
                let val tenv' = (p := SOME ty'; 
                                 S.enter(tenv,name,actualTy (T.NAME(name,p))))
                in processMutrecTypeBodys(tenv',A.TypeDec ds,passedRecOrArr',
                                          pos)
                end
            | SOME ty'' => 
                let val tenv' = S.enter(tenv,name,ty')
                in processMutrecTypeBodys(tenv',A.TypeDec ds,passedRecOrArr',
                                          pos)
                end
            | NONE => (ErrorMsg.error pos "Undefined type."; tenv))
          end
      | _ => tenv
  fun processMutrecFunHeaders(venv,tenv,dec,lev) =
      case dec of 
        A.FunctionDec[] => venv
      | A.FunctionDec({name,params,body,pos,result}::fs) =>
          let val formals = map #ty (transParams(params,tenv))
              val result = transResult(result,tenv)
              val label = Temp.newlabel()
              fun makeList(x,0) = []
              |   makeList(x,n) = x::makeList(x,n-1)
              val newLevel = Tr.newLevel{parent=lev, name=label, 
                                         formals=makeList(false,length params)}
              val funentry = E.FunEntry{level=newLevel,label=label,
                                        formals=formals,result=result}
              val venv' = S.enter(venv,name,funentry)
          in processMutrecFunHeaders(venv',tenv,A.FunctionDec fs,lev)
          end
      | _ => venv
  fun transDec(venv, tenv, dec, break, lev) = 
    let fun processMutrecFunBodys(venv,tenv,dec,break,lev) =
          (case dec of 
            A.FunctionDec[] => venv
          | A.FunctionDec({name,params,body,pos,result}::fs) => 
              let val lev' = case nonoptV(S.look(venv,name), pos,lev) of 
                      E.FunEntry{level=x,...} => x
                    | _ => (ErrorMsg.error pos "Expected FunEntry got VarEntry"; 
                            Tr.newLevel{parent=lev, name=Temp.newlabel(), 
                                        formals=[]})
                  val paramsAndAccs = ListPair.zip(transParams(params,tenv),
                                                   Tr.formals lev')
                  fun enterparam (({name,ty},acc),venv) = 
                    S.enter(venv,name, E.VarEntry{access=acc,ty=ty})
                  val venv' = foldr enterparam venv paramsAndAccs
                  val {exp=bodyTrans, ty=bodyTy} = 
                    transExp(venv',tenv,break,lev') body
                  val resultTy = transResult(result,tenv)
                  val bodyViewShift = Tr.fundec(bodyTrans,lev')
              in if bodyTy = resultTy
                 then (Tr.procEntryExit{level=lev,body=bodyViewShift};
                    processMutrecFunBodys(venv,tenv,A.FunctionDec fs,break,lev))
                 else (ErrorMsg.error pos 
                      "Function return type does not match its declaration"; 
                      venv)
              end
          | _ => venv)
    in case dec of 
         A.VarDec{name, escape, typ=NONE, init, pos} =>
           let val {exp,ty} = transExp(venv,tenv,break,lev) init
               val access = Tr.allocLocal lev true
               val entry = E.VarEntry{access=access,ty=ty}
           in {tenv=tenv, venv=S.enter(venv,name,entry), 
               exps=[Tr.assign(access,exp)]}
           end
       | A.VarDec{name, escape, typ=SOME(sym,_), init, pos} =>
           let val {exp,ty} = transExp(venv,tenv,break,lev) init
               val expected = nonoptT(S.look(tenv,sym), pos)
               val freeVar = A.VarDec{name=name, escape=escape, typ=NONE,
                                      init=init, pos=pos}
           in if expected = ty 
              then transDec(venv,tenv,freeVar,break,lev)          
              else  (ErrorMsg.error pos 
                    ("Variable type does not match expression result"); 
                    {tenv=tenv,venv=venv,exps=[]})
           end
       | A.TypeDec[] => {venv=venv,tenv=tenv,exps=[]}
       | typeDec as A.TypeDec _ => 
           let val tenv' = processMutrecTypeHeaders(tenv,typeDec)
               val tenv'' = processMutrecTypeBodys(tenv',typeDec,false,0)
           in {tenv=tenv'',venv=venv,exps=[]}
           end
       | A.FunctionDec[] => {venv=venv,tenv=tenv,exps=[]}
       | funDec as A.FunctionDec _ =>
           let val venv' = processMutrecFunHeaders(venv,tenv,funDec,lev)
               val venv'' = processMutrecFunBodys(venv',tenv,funDec,break,lev) 
           in {tenv=tenv,venv=venv'',exps=[]}
           end
    end
  and transExp(venv,tenv,break,lev) =
    let fun trexp e = 
          case e of
            A.VarExp(var) => trvar var
          | A.NilExp => {exp=Tr.nil(), ty=T.NIL}
          | A.IntExp(num) => {exp=Tr.int(num), ty=T.INT}
          | A.StringExp(lit,_) => {exp=Tr.string lit, ty=T.STRING}
          | A.CallExp{func,args,pos} => 
              let fun comp(arg,formal) = 
                if #ty (trexp arg) = formal then () else ErrorMsg.error pos 
                 ("Type mismatch between function formal parameters and args")
              in case S.look(venv,func) of
                  SOME (E.FunEntry{formals,result,...}) =>
                    (app comp (ListPair.zipEq(args,formals))
                      handle UnequalLengths => ErrorMsg.error pos 
                         ("Invalid number of arguments in function call");
                          {exp=Tr.dummy(),ty=result})
                  | _ => {exp=Tr.dummy(),ty=T.NAME(func,ref NONE)}
              end
          | A.OpExp{left,oper,right,pos} => 
              let val l as {exp=leftTrans,...}= trexp left
                  val r as {exp=rightTrans,...}= trexp right
                  val ty = checkeqty(l,r,pos)
                  val exp = Tr.oper(leftTrans,oper,rightTrans)
              in case (oper,ty) of 
                   (((A.PlusOp|A.MinusOp|A.TimesOp|A.DivideOp),T.INT) | 
                    ((A.LtOp|A.LeOp|A.GtOp|A.GeOp),(T.INT|T.STRING)) |
                    ((A.EqOp|A.NeqOp),(T.INT|T.STRING|T.RECORD _ |T.ARRAY _))) 
                       => {exp=exp,ty=ty}
                 | _ => invalidComp pos
              end
          | A.SeqExp((exp,_)::[]) => trexp exp
          | A.SeqExp(_::exps) => transExp(venv,tenv,break,lev) (A.SeqExp exps)
          | A.LetExp{decs,body,pos} =>
              let fun transDec' (dec, {venv=venv',tenv=tenv',exps=exps'}) = 
                    transDec(venv',tenv',dec,break,lev)
                  val {venv=venv',tenv=tenv',exps=exps'} = 
                    foldl transDec' {venv=venv,tenv=tenv,exps=[]} decs
              in transExp(venv',tenv',break,lev) body
              end
          | A.RecordExp{fields=[],typ,pos} => 
              {exp=Tr.dummy(), ty=nonoptT(S.look(tenv, typ),pos)}
          | A.RecordExp{fields=(symbol, exp, recpos)::xs,typ,pos} =>
              (validateVarT(venv,symbol,exp,recpos);
               trexp(A.RecordExp{fields=xs, typ=typ, pos=pos}))
          | A.AssignExp{var,exp,pos} => (checkeqty(trvar var, trexp exp, pos); 
              {exp=Tr.dummy(),ty=T.UNIT})
          | A.IfExp{test, then', else', pos} =>
              let val thenExpty = trexp then'
                  val ty' = case else' of 
                              SOME elseExp => 
                                checkeqty(thenExpty, trexp elseExp, pos)
                            | NONE => #ty thenExpty
              in (checkint (trexp test, pos); {exp=Tr.dummy(), ty=ty'})
              end
          | A.WhileExp{test,body,pos} =>
              (checkint (trexp test, pos); 
               {exp=Tr.dummy(),ty=checkeqty((transExp(venv,tenv,break+1,lev) body), 
                                     {exp=Tr.dummy(), ty=T.UNIT}, pos)})
          | A.BreakExp(pos) => if break > 0 then {exp=Tr.dummy(), ty=T.UNIT} else 
              (ErrorMsg.error pos ("Break not enclosed in loop.");
               {exp=Tr.dummy(),ty=T.UNIT})
          | A.ArrayExp{typ,size,init,pos} =>
              (case nonoptT(S.look(tenv,typ),pos) of
                arr as T.ARRAY(expectedTy,_) => 
                  let val init' as {exp=initTrans,ty=initTy} = trexp init 
                      val {exp=sizeTrans,...} = trexp size
                  in (checkeqty(init',{exp=Tr.dummy(),ty=expectedTy},pos); 
                      {exp=Tr.array(initTrans,sizeTrans,lev),ty=arr})
                  end
              | _ => (ErrorMsg.error pos ("Array expected.");
                      {exp=Tr.dummy(),ty=T.UNIT}))
          | A.ForExp{var,escape,lo,hi,body,pos} =>
              let val vd = A.VarDec{name=var, escape=ref true, typ=NONE,
                                    init=lo, pos=pos}
                  val {venv=venv',tenv=tenv',exps=exps'} = 
                    transDec(venv, tenv, vd, break, lev)
              in (checkint(trexp lo, pos); checkint(trexp hi, pos); 
                  {exp=Tr.dummy(),ty=checkeqty(transExp(venv', tenv',break+1,lev) body, 
                                    {exp=Tr.dummy(), ty=T.UNIT}, pos)})
              end
          | _ =>
              {exp=Tr.dummy(), ty=T.UNIT}
        and trvar e =
          case e of
            A.SimpleVar(id,pos) =>
              let val access = case nonoptV(S.look(venv,id),pos,lev) of 
                                  E.VarEntry{access,...} => access
                                | E.FunEntry{result,...} => 
                                    (ErrorMsg.error pos 
                                      ("Variable not declared in this scope") ; 
                                    Tr.allocLocal lev true)
              in {exp=Tr.simpleVar(access,lev),ty= nonoptT(S.look(tenv,id),pos)}
              end
          | A.FieldVar(inVar,sym,pos) =>
              let val {exp=inVarTrans,ty=varTy} = trvar inVar
                  fun fl_look([],target,_) = 
                        (ErrorMsg.error pos 
                        ("Record field " ^ S.name target ^ " type not defined");
                        {i=0, ty=T.UNIT})
                    | fl_look((currSym,currTy)::xs,target,i) = 
                        if currSym = target 
                        then {i=(~1), ty=currTy}
                        else fl_look(xs,target,i+1)
              in case varTy of 
                   T.RECORD(fl,_) => let val {i,ty} = fl_look(fl,sym,0) 
                                     in {exp=Tr.fieldVar(inVarTrans,i),ty=ty}
                                     end 
                 | _ => (ErrorMsg.error pos ("Field dot suffix cannot be " ^
                                             "applied to a non-record type " ^
                                             "variable");
                        {exp=Tr.dummy(), ty=T.UNIT})
              end
          | A.SubscriptVar(inVar,exp,pos) => case #ty (trvar inVar) of 
                T.ARRAY(ty,_) =>
                 let val exp' as {exp=expTrans,...} = trexp exp
                     val {exp=inVarTrans,...} = trvar inVar
                 in (checkint(exp', pos); 
                     {exp=Tr.subscriptVar(inVarTrans,expTrans),ty=ty})
                 end
              | _ => (ErrorMsg.error pos 
                     ("Subscript suffix cannot be applied to a non-array " ^ 
                      "type variable");
                     {exp=Tr.dummy(),ty=T.UNIT})
        and validateVarT(venv, symbol, exp, pos) =
          let val ty' = case nonoptV(S.look(venv, symbol),pos,lev) of
                          E.VarEntry{ty,...} => ty
                        | E.FunEntry{result,...} => result 
          in checkeqty({exp=Tr.dummy(),ty=ty'}, trexp exp, pos)
          end
    in trexp
    end
  
  fun transProg exp = (transExp(E.base_venv, E.base_tenv, 0, Tr.outermost) exp; 
                       Tr.getResult())
(*
  fun printTree exp = Printtree.printtree(
    #exp (transExp(E.base_venv, E.base_tenv, 0, Tr.outermost) exp))
    *)
end