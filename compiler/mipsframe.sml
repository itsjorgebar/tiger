signature FRAME =
sig 
    type frame
    type access
    val FP : Temp.temp
    val RV : Temp.temp
    val wordSize: int
    val newFrame : {name: Temp.label,
                    formals: bool list} -> frame
    val name : frame -> Temp.label
    val formals : frame -> access list
    val allocLocal : frame -> bool -> access
    val exp : access -> Tree.exp -> Tree.exp
    val procEntryExit1 : frame * Tree.stm -> Tree.stm
    datatype frag = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string
    val calleePreserve : access list
end

structure MipsFrame : FRAME = 
struct
    (*TODO add view shift*)
    datatype access = InFrame of int | InReg of Temp.temp
    type frame = {formals: access list, shift: unit, locals: int ref, 
                  name: Temp.label}
    datatype frag = PROC of {body: Tree.stm, frame: frame}
                  | STRING of Temp.label * string

    val wordSize = 32
    val calleePreserve = map (fn x => InReg x) 
      [16,17,18,19,20,21,22,23,28,29,30,31]
    fun newFrame{name=name,formals=formals} = 
        let fun locateFormals 0 = [InFrame 0]
            |   locateFormals n = InFrame(n)::locateFormals(n-1)
        in {formals=locateFormals(length formals) , shift=(), locals= ref 0, 
            name=name}
        end
    fun name ({name=name,...} : frame) =  name
    fun formals ({formals=formals,...} : frame) =  formals
    fun allocLocal ({locals=locals,...} : frame) esc = 
        (locals := !locals + 1; 
         if esc then InReg(Temp.newtemp()) else InFrame(~ (!locals)))
    val FP = 30
    val RV = 31 
    fun procEntryExit1(frame, body) = body (* Stub *)
    fun exp (InReg k) _ = Tree.MEM(Tree.TEMP k)
    |   exp (InFrame k) fp =  Tree.MEM(Tree.BINOP(Tree.PLUS,fp, Tree.CONST k))
end