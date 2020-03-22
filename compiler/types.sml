structure Types =
struct

  type unique = unit ref

  datatype ty = 
            RECORD of (Symbol.symbol * ty) list * unique
          | NIL
          | INT
          | STRING
          | ARRAY of ty * unique
	  | NAME of Symbol.symbol * ty option ref
	  | UNIT

  fun tyToStr typ = case typ of
     RECORD a => "RECORD"
  |  NIL => "NIL"
  |  INT => "INT"
  |  STRING => "STRING"
  |  ARRAY a => "ARRAY"
  |  NAME a => "NAME"
  |  UNIT => "UNIT"

end

