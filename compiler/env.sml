structure T = Types

signature ENV =
sig
  type access
  type ty
  datatype enventry = VarEntry of {access: Translate.access, ty: ty}
                    | FunEntry of {level: Translate.level,
                                   label: Temp.label, 
                                   formals: ty list, result: ty}
  val base_tenv : ty Symbol.table (* predefined types*)
  val base_venv : enventry Symbol.table (* predefined functions*)
end

structure Env: ENV =
struct
  type access = unit (*TODO: change *)
  type ty = T.ty
  datatype enventry = VarEntry of {access: Translate.access, ty: ty}
                    | FunEntry of {level: Translate.level,
                                   label: Temp.label, 
                                   formals: ty list, result: ty}

  val base_tenv = 
    Symbol.enter(
      Symbol.enter(
        Symbol.empty,
        Symbol.symbol "int", 
        T.INT
      ),
      Symbol.symbol "string",
      T.STRING
    )
    
  (* Initializes venv with standard library (sl) *)
  fun fill_venv ([], tbl) = tbl
    | fill_venv ((id,frmls,res)::t, tbl) =
        fill_venv(
          t,
          Symbol.enter(
            tbl,
            Symbol.symbol id, 
            FunEntry{level=Translate.outermost,label=Temp.newlabel(), 
                     formals=frmls,result=res}
          )
        )

  val sl = [ ("print", [T.STRING], T.UNIT),
              ("flush", [T.UNIT], T.UNIT),
              ("getchar", [T.UNIT], T.STRING),
              ("ord", [T.STRING], T.INT),
              ("chr", [T.INT], T.STRING),
              ("size", [T.STRING], T.INT),
              ("substring", [T.STRING,T.INT, T.INT], T.STRING),
              ("concat", [T.STRING, T.STRING], T.STRING),
              ("not", [T.INT], T.INT),
              ("exit", [T.INT], T.UNIT)
            ]

  val base_venv = fill_venv(sl,Symbol.empty)
end

