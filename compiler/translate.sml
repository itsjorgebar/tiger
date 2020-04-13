signature TRANSLATE =
sig
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
    datatype level = Top | Lv of Frame.frame
    type access = level * Frame.access
    val outermost = Top
    fun newLevel(parent,name,formals) = Frame.newFrame(name, true::formals)
    fun formals (Lv fr) = formals fr
    |   formals _ = [] (* Unreachable *)
    fun allocLocal (lev as Lv fr) bool = lev * (Frame.allocLocal fr bool)
    |   allocLocal x _ = x * InFrame(0) (* Unreachable *)
end