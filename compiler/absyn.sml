structure Absyn = 
struct

type pos = int   and   symbol = Symbol.symbol

datatype var = SimpleVar of symbol * pos
            | FieldVar of var * symbol * pos
            | SubscriptVar of var * exp * pos

and exp = VarExp of var
        | NilExp
        | IntExp of int
        | StringExp of string * pos
        | CallExp of {func: symbol, args: exp list, pos: pos}
        | OpExp of {left: exp, oper: oper, right: exp, pos: pos}
        | RecordExp of {fields: (symbol * exp * pos) list, typ: symbol, 
                        pos: pos}
        | SeqExp of (exp * pos) list
        | AssignExp of {var: var, exp: exp, pos: pos}
        | IfExp of {test: exp, then': exp, else': exp option, pos: pos}
        | WhileExp of {test: exp, body: exp, pos: pos}
	      | ForExp of {var: symbol, escape: bool ref, lo: exp, hi: exp, body: exp,
                     pos: pos}
        | BreakExp of pos
        | LetExp of {decs: dec list, body: exp, pos: pos}
        | ArrayExp of {typ: symbol, size: exp, init: exp, pos: pos}

and dec = FunctionDec of fundec list
        | VarDec of {name: symbol, escape: bool ref, typ: (symbol * pos) option,
		                 init: exp, pos: pos}
        | TypeDec of {name: symbol, ty: ty, pos: pos} list

and ty = NameTy of symbol * pos
       | RecordTy of field list
       | ArrayTy of symbol * pos

and oper = PlusOp | MinusOp | TimesOp | DivideOp
         | EqOp | NeqOp | LtOp | LeOp | GtOp | GeOp

withtype field = {name: symbol, escape: bool ref, typ: symbol, pos: pos}
   and fundec = {name: symbol, params: field list, 
                 result: (symbol * pos) option, body: exp, pos: pos}

fun forAlt(var,lo,hi,body,pos) = 
  let val limit = Symbol.symbol "limit" 
      val condition = IfExp{test=OpExp{left=VarExp(SimpleVar(var,pos)),
                                       oper=LtOp,
                                       right=VarExp(SimpleVar(limit,pos)),
                                       pos=pos},
                            then' =BreakExp pos,
                            else' =NONE,
                            pos=pos}
  in LetExp{decs=[VarDec{name=var,escape=ref true,typ=NONE,init=lo,pos=pos},
                  VarDec{name=limit,escape=ref true,typ=NONE,init=hi,pos=pos}],
            body=IfExp{test=OpExp{left=lo,oper=GtOp,right=hi,pos=pos},
                       then' =NilExp,
                       else' =SOME(WhileExp{test=IntExp 1,
                                            body=SeqExp[(body,pos),
                                                        (condition,pos)],
                                            pos=pos}),
                       pos=pos},
            pos=pos}
  end
                    
end
        
