structure Fr = Frame

signature TRANSLATE =
sig
    type exp
    type level
    type access (* not the same as Frame.access *)
    val outermost : level
    val newLevel : {parent: level, name: Temp.label,
                    formals: bool list} -> level
    val formals: level -> access list
    val allocLocal: level -> bool -> access
end

structure Translate : TRANSLATE = 
struct
    datatype exp = 
          Ex of Tree.exp
        | Nx of Tree.stm
        | Cx of Temp.label * Temp.label -> Tree.stm
    datatype level = Top | Lv of Fr.frame
    type access = level * Fr.access
    val outermost = Top
    fun newLevel{parent=parent,name=name,formals=formals} = 
        Lv (Fr.newFrame{name=name, formals=true::formals})
    fun formals (lev as Lv fr) = map (fn frAcc => (lev,frAcc)) (Fr.formals fr)
    |   formals _ = [] (* Unreachable *)
    fun allocLocal (lev as Lv fr) esc = (lev,Fr.allocLocal fr esc)
    |   allocLocal lev esc = (* Unreachable *)
          (lev,Fr.allocLocal (Fr.newFrame{name=Temp.newlabel(),formals=[]}) esc)
end