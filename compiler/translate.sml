signature TRANSLATE =
sig
    structure Frame : FRAME (*TODO find out if this line is necessary*)
    type exp
    type level
    type access (* not the same as Frame.access *)
    type frag

    val outermost : level
    val newLevel : {parent: level, name: Temp.label,
                    formals: bool list} -> level
    val formals: level -> access list
    val allocLocal: level -> bool -> access

    val procEntryExit : {level: level, body: exp} -> unit
    val getResult : unit -> Frame.frag list

    val unEx : exp -> Tree.exp
    val unNx : exp -> Tree.stm
    val unCx : exp -> (Temp.label * Temp.label -> Tree.stm)

    val simpleVar : access * level -> exp
    val fieldVar : access * level -> exp (*TODO correct*)
    val subscriptVar : access * level -> exp(*TODO correct*)
    val assign : access * exp -> exp
end

structure Translate : TRANSLATE = 
struct
    structure Frame = MipsFrame
    structure T = Tree
    datatype exp = 
          Ex of T.exp
        | Nx of T.stm
        | Cx of Temp.label * Temp.label -> T.stm
    datatype level = Top | Lv of Frame.frame * unit ref
    type access = level * Frame.access
    type frag = Frame.frag (*TODO set it correctly*)
    val outermost = Top
    fun newLevel{parent=parent,name=name,formals=formals} = 
        Lv(Frame.newFrame{name=name, formals=true::formals}, ref ())
    fun formals (lev as Lv(fr,_)) = map (fn frAcc => (lev,frAcc)) 
                                                        (Frame.formals fr)
    |   formals _ = [] (* Unreachable *)
    fun allocLocal (lev as Lv(fr,_)) esc = (lev,Frame.allocLocal fr esc)
    |   allocLocal lev esc = (* Unreachable *)
          (lev,Frame.allocLocal (Frame.newFrame{name=Temp.newlabel(),
                                                formals=[]}) esc)
(*
    fun transAcc Frame.InFrame k = T.CONST k 
    |   transAcc Frame.InReg k = T.TEMP k
 *)

    fun procEntryExit {level=lev, body=exp} = ()(*TODO implement it*) 
    fun getResult() = [] (*TODO implement it*) 

    fun seq [] = T.EXP (T.CONST 0) (* Unreachable *)
    |   seq (x::[]) = x
    |   seq (x::xs) = T.SEQ(x,seq xs)

    fun unEx (Ex e) = e
      | unEx (Nx s) = T.ESEQ(s,T.CONST 0)
      | unEx (Cx genstm) =
            let val r = Temp.newtemp()
                val t = Temp.newlabel()
                val f = Temp.newlabel()
            in T.ESEQ(seq[T.MOVE(T.TEMP r, T.CONST 1),
                  genstm(t,f),
                  T.LABEL f,
                  T.MOVE(T.TEMP r, T.CONST 0),
                  T.LABEL t],
               T.TEMP r)
            end
    
    fun unNx (Ex e) = T.EXP e
    |   unNx (Nx s) = s
    |   unNx (cond as Cx genstm) = T.EXP(unEx cond) 

    fun unCx (Ex (T.CONST 0)) = (fn(t,f) => T.JUMP(T.NAME f,[f]))
    |   unCx (Ex (T.CONST 1)) = (fn(t,f) => T.JUMP(T.NAME t,[t]))
    |   unCx (Ex e) = (fn(t,f) => T.CJUMP(T.EQ,e,T.CONST 0,f,t))
    |   unCx (Nx _) = (fn(t,f) => T.JUMP(T.NAME t,[t])) (* Unreachable *)
    |   unCx (Cx genstm) = genstm

    fun simpleVar((def_lev,f_acc),call_lev) = 
        let fun getDefFP(curr_lev, curr_fp) = 
                if def_lev = curr_lev then curr_fp
                else let val (parent_lev,parent_acc) = hd (formals curr_lev)
                         val parent_fp = Frame.exp parent_acc curr_fp
                     in getDefFP(parent_lev,parent_fp) 
                     end
        in Ex(Frame.exp f_acc (getDefFP(call_lev, T.TEMP Frame.FP)))
        end

    fun assign((_,f_acc),exp) = 
      Nx(T.MOVE((Frame.exp f_acc (T.TEMP Frame.FP)),unEx exp))

    (*TODO, add a fun for each type of A.var and A.exp *)
end