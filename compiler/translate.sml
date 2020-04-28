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
    val getResult : unit -> frag list

    val unEx : exp -> Tree.exp
    val unNx : exp -> Tree.stm
    val unCx : exp -> (Temp.label * Temp.label -> Tree.stm)

    val dummy : unit -> exp
    val simpleVar : access * level -> exp
    val fieldVar : exp * int -> exp
    val subscriptVar : exp * exp -> exp

    val int : int -> exp
    val string : string -> exp
    val array : exp * exp * level -> exp

    val assign : access * exp -> exp
    val fundec : exp * level -> exp
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
    type frag = Frame.frag
    val outermost = Top
    val result : frag list ref = ref []
    val fp = T.TEMP Frame.FP
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

    fun dummy() = Ex(T.CONST 1)

    fun simpleVar((def_lev,f_acc),call_lev) = 
        let fun getDefFP(curr_lev, curr_fp) = 
                if def_lev = curr_lev then curr_fp
                else let val (parent_lev,parent_acc) = hd (formals curr_lev)
                         val parent_fp = Frame.exp parent_acc curr_fp
                     in getDefFP(parent_lev,parent_fp) 
                     end
        in Ex(Frame.exp f_acc (getDefFP(call_lev, T.TEMP Frame.FP)))
        end

    fun structuredVar(inVar,numExp) = Ex(T.MEM(
      T.BINOP(T.PLUS,
        unEx inVar,
        T.BINOP(T.MUL,
          numExp,
          T.CONST Frame.wordSize))))

    fun fieldVar(inVar,i) = structuredVar(inVar, T.CONST i)

    fun subscriptVar(inVar,exp) = structuredVar(inVar, unEx exp)

    fun int num = Ex(T.CONST num)
    fun string lit = let val lab = Temp.newlabel()
                     in (result := (Frame.STRING(lab,lit)::(!result));
                         Ex(T.NAME lab)) 
                     end

    fun array(init,size,lev) = dummy() (*TODO, prototype let val (_,p) = allocLocal(lev,true)
                           in Ex(Frame.exp p fp)
                           end *)

    fun assign((_,f_acc),exp) = 
      Nx(T.MOVE((Frame.exp f_acc (T.TEMP Frame.FP)),unEx exp))

    fun getFrame (Lv(f,_)) = f
              (* Unreachable *)
    |   getFrame _ = Frame.newFrame{name=Temp.newlabel(),formals=[]}

    fun fundec(bodyEx,lev) =
          (*  View shift prototype
          val dummyMem = T.CONST 0
          val mallocSaveRegs = List.tabulate(length Frame.calleePreserve,
                                        (fn _ => #2(allocLocal lev true)))
          val mallocSaveRegsTrans = map (fn mem => Frame.exp mem 
                                                             (T.TEMP Frame.FP)) 
                                        mallocSaveRegs
          val calleePreserveTrans = map (fn mem => Frame.exp mem dummyMem) 
                                        Frame.calleePreserve
          fun moveMulti(src,dst) = seq(map (fn (src,dst) => T.MOVE(dst,src))
                                           (ListPair.zip(src,dst))) 

          val saveRegs =  moveMulti(calleePreserveTrans,mallocSaveRegsTrans)
          val restoreRegs = moveMulti(mallocSaveRegsTrans,calleePreserveTrans)
          *)
          
          (* 1 (later) *)
          (* 2 (later) *)
          (* 3 (later) *)
          (* view shift: 4,5,6 *)
          (* 7 *)
          (* val returnVal = T.MOVE(T.TEMP Frame.RV, unEx bodyEx) *)
          (* 8 (later) *)
          (* 9 (later) *)
          (* 10 (later) *)
          (* 11 (later) *)
          Nx(Frame.procEntryExit1(getFrame lev, unNx bodyEx))

    fun getResult() = !result

    fun procEntryExit{level,body} = 
      (result := Frame.PROC{body=unNx body,frame=getFrame level}::getResult();
       ())

    (*TODO, add a fun for each type of A.var and A.exp *)
end