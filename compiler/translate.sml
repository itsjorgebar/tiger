signature TRANSLATE =
sig
    structure Frame : FRAME (*TODO find out if this line is necessary*)
    type exp
    type level
    type access (* not the same as Frame.access *)
    val outermost : level
    val newLevel : {parent: level, name: Temp.label,
                    formals: bool list} -> level
    val formals: level -> access list
    val allocLocal: level -> bool -> access

    val simpleVar : access * level -> exp
    val procEntryExit : {level: level, body: exp} -> unit
    val getResult : unit -> Frame.frag list
end

structure Translate : TRANSLATE = 
struct
    structure Frame = MipsFrame
    datatype exp = 
          Ex of Tree.exp
        | Nx of Tree.stm
        | Cx of Temp.label * Temp.label -> Tree.stm
    datatype level = Top | Lv of Frame.frame * unit ref
    type access = level * Frame.access
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
    fun transAcc Frame.InFrame k = Tree.CONST k 
    |   transAcc Frame.InReg k = Tree.TEMP k
 *)

    (*TODO, chain of Mem(+(static link, offset)) *)
    fun simpleVar((def_lev,f_acc),call_lev) = 
        let fun getDefFP(curr_lev, curr_fp) = 
                if def_lev = curr_lev then curr_fp
                else let val (parent_lev,parent_acc) = #1 (formals curr_lev)
                         val parent_fp = Frame.exp parent_acc curr_fp
                     in getDefFP(parent_lev,parent_fp) 
                     end
        in Ex(Frame.exp f_acc getDefFP(call_lev, Tree.TEMP Frame.FP))
        end

    (*TODO, add a fun for each type of A.var and A.exp *)
end