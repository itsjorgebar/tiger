functor TigerLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Tiger_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct

end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\026\000\003\000\025\000\004\000\024\000\008\000\023\000\
\\009\000\051\000\016\000\022\000\029\000\021\000\032\000\020\000\
\\033\000\019\000\040\000\018\000\041\000\017\000\000\000\
\\001\000\002\000\026\000\003\000\025\000\004\000\024\000\008\000\023\000\
\\016\000\022\000\029\000\021\000\032\000\020\000\033\000\019\000\
\\040\000\018\000\041\000\017\000\000\000\
\\001\000\002\000\045\000\000\000\
\\001\000\002\000\056\000\000\000\
\\001\000\002\000\066\000\000\000\
\\001\000\002\000\091\000\000\000\
\\001\000\009\000\064\000\000\000\
\\001\000\009\000\077\000\000\000\
\\001\000\011\000\070\000\015\000\044\000\016\000\043\000\017\000\042\000\
\\018\000\041\000\019\000\040\000\020\000\039\000\021\000\038\000\
\\022\000\037\000\023\000\036\000\024\000\035\000\025\000\034\000\
\\026\000\033\000\000\000\
\\001\000\011\000\076\000\015\000\044\000\016\000\043\000\017\000\042\000\
\\018\000\041\000\019\000\040\000\020\000\039\000\021\000\038\000\
\\022\000\037\000\023\000\036\000\024\000\035\000\025\000\034\000\
\\026\000\033\000\000\000\
\\001\000\013\000\090\000\000\000\
\\001\000\015\000\044\000\016\000\043\000\017\000\042\000\018\000\041\000\
\\019\000\040\000\020\000\039\000\021\000\038\000\022\000\037\000\
\\023\000\036\000\024\000\035\000\025\000\034\000\026\000\033\000\
\\030\000\063\000\000\000\
\\001\000\015\000\044\000\016\000\043\000\017\000\042\000\018\000\041\000\
\\019\000\040\000\020\000\039\000\021\000\038\000\022\000\037\000\
\\023\000\036\000\024\000\035\000\025\000\034\000\026\000\033\000\
\\034\000\079\000\000\000\
\\001\000\015\000\044\000\016\000\043\000\017\000\042\000\018\000\041\000\
\\019\000\040\000\020\000\039\000\021\000\038\000\022\000\037\000\
\\023\000\036\000\024\000\035\000\025\000\034\000\026\000\033\000\
\\035\000\062\000\000\000\
\\001\000\015\000\044\000\016\000\043\000\017\000\042\000\018\000\041\000\
\\019\000\040\000\020\000\039\000\021\000\038\000\022\000\037\000\
\\023\000\036\000\024\000\035\000\025\000\034\000\026\000\033\000\
\\035\000\089\000\000\000\
\\001\000\019\000\075\000\000\000\
\\001\000\019\000\093\000\000\000\
\\001\000\027\000\061\000\000\000\
\\001\000\039\000\082\000\000\000\
\\097\000\015\000\044\000\016\000\043\000\017\000\042\000\018\000\041\000\
\\019\000\040\000\020\000\039\000\021\000\038\000\022\000\037\000\
\\023\000\036\000\024\000\035\000\025\000\034\000\026\000\033\000\000\000\
\\098\000\000\000\
\\099\000\000\000\
\\100\000\010\000\029\000\014\000\028\000\027\000\027\000\000\000\
\\101\000\000\000\
\\102\000\000\000\
\\103\000\000\000\
\\104\000\000\000\
\\105\000\000\000\
\\106\000\000\000\
\\107\000\000\000\
\\108\000\000\000\
\\109\000\000\000\
\\110\000\000\000\
\\111\000\000\000\
\\112\000\000\000\
\\113\000\000\000\
\\114\000\000\000\
\\115\000\000\000\
\\116\000\000\000\
\\117\000\000\000\
\\118\000\007\000\065\000\015\000\044\000\016\000\043\000\017\000\042\000\
\\018\000\041\000\019\000\040\000\020\000\039\000\021\000\038\000\
\\022\000\037\000\023\000\036\000\024\000\035\000\025\000\034\000\
\\026\000\033\000\000\000\
\\121\000\000\000\
\\122\000\005\000\078\000\015\000\044\000\016\000\043\000\017\000\042\000\
\\018\000\041\000\019\000\040\000\020\000\039\000\021\000\038\000\
\\022\000\037\000\023\000\036\000\024\000\035\000\025\000\034\000\
\\026\000\033\000\000\000\
\\123\000\015\000\044\000\016\000\043\000\017\000\042\000\018\000\041\000\
\\019\000\040\000\020\000\039\000\021\000\038\000\022\000\037\000\
\\023\000\036\000\024\000\035\000\025\000\034\000\026\000\033\000\000\000\
\\124\000\015\000\044\000\016\000\043\000\017\000\042\000\018\000\041\000\
\\019\000\040\000\020\000\039\000\021\000\038\000\022\000\037\000\
\\023\000\036\000\024\000\035\000\025\000\034\000\026\000\033\000\000\000\
\\125\000\015\000\044\000\016\000\043\000\017\000\042\000\018\000\041\000\
\\019\000\040\000\020\000\039\000\021\000\038\000\022\000\037\000\
\\023\000\036\000\024\000\035\000\025\000\034\000\026\000\033\000\000\000\
\\126\000\000\000\
\\127\000\000\000\
\\128\000\000\000\
\\129\000\000\000\
\\130\000\000\000\
\\131\000\000\000\
\\132\000\000\000\
\\133\000\000\000\
\\134\000\000\000\
\\135\000\000\000\
\\136\000\000\000\
\\137\000\000\000\
\\138\000\017\000\042\000\018\000\041\000\000\000\
\\139\000\000\000\
\\140\000\000\000\
\\141\000\000\000\
\\142\000\005\000\087\000\015\000\044\000\016\000\043\000\017\000\042\000\
\\018\000\041\000\019\000\040\000\020\000\039\000\021\000\038\000\
\\022\000\037\000\023\000\036\000\024\000\035\000\025\000\034\000\
\\026\000\033\000\000\000\
\\143\000\015\000\044\000\016\000\043\000\017\000\042\000\018\000\041\000\
\\019\000\040\000\020\000\039\000\021\000\038\000\022\000\037\000\
\\023\000\036\000\024\000\035\000\025\000\034\000\026\000\033\000\000\000\
\\161\000\008\000\054\000\010\000\053\000\012\000\052\000\000\000\
\\162\000\000\000\
\\163\000\000\000\
\\164\000\015\000\044\000\016\000\043\000\017\000\042\000\018\000\041\000\
\\019\000\040\000\020\000\039\000\021\000\038\000\022\000\037\000\
\\023\000\036\000\024\000\035\000\025\000\034\000\026\000\033\000\000\000\
\\165\000\015\000\044\000\016\000\043\000\017\000\042\000\018\000\041\000\
\\019\000\040\000\020\000\039\000\021\000\038\000\022\000\037\000\
\\023\000\036\000\024\000\035\000\025\000\034\000\026\000\033\000\000\000\
\\166\000\015\000\044\000\016\000\043\000\017\000\042\000\018\000\041\000\
\\019\000\040\000\020\000\039\000\021\000\038\000\022\000\037\000\
\\023\000\036\000\024\000\035\000\025\000\034\000\026\000\033\000\
\\031\000\080\000\000\000\
\\167\000\015\000\044\000\016\000\043\000\017\000\042\000\018\000\041\000\
\\019\000\040\000\020\000\039\000\021\000\038\000\022\000\037\000\
\\023\000\036\000\024\000\035\000\025\000\034\000\026\000\033\000\000\000\
\\168\000\015\000\044\000\016\000\043\000\017\000\042\000\018\000\041\000\
\\019\000\040\000\020\000\039\000\021\000\038\000\022\000\037\000\
\\023\000\036\000\024\000\035\000\025\000\034\000\026\000\033\000\000\000\
\"
val actionRowNumbers =
"\002\000\036\000\035\000\034\000\
\\033\000\032\000\031\000\030\000\
\\029\000\027\000\028\000\026\000\
\\038\000\023\000\020\000\021\000\
\\037\000\003\000\002\000\002\000\
\\002\000\001\000\025\000\024\000\
\\065\000\002\000\004\000\002\000\
\\002\000\002\000\002\000\058\000\
\\057\000\056\000\055\000\054\000\
\\053\000\052\000\051\000\050\000\
\\049\000\048\000\047\000\018\000\
\\014\000\012\000\059\000\007\000\
\\041\000\060\000\005\000\002\000\
\\002\000\068\000\066\000\009\000\
\\044\000\045\000\046\000\002\000\
\\002\000\002\000\022\000\002\000\
\\016\000\010\000\008\000\043\000\
\\067\000\013\000\071\000\070\000\
\\040\000\002\000\019\000\039\000\
\\002\000\002\000\002\000\063\000\
\\002\000\042\000\015\000\069\000\
\\011\000\006\000\064\000\002\000\
\\061\000\017\000\072\000\002\000\
\\063\000\062\000\000\000"
val gotoT =
"\
\\001\000\014\000\002\000\094\000\011\000\013\000\012\000\012\000\
\\013\000\011\000\017\000\010\000\018\000\009\000\019\000\008\000\
\\021\000\007\000\023\000\006\000\024\000\005\000\025\000\004\000\
\\026\000\003\000\030\000\002\000\031\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\030\000\015\000\029\000\016\000\028\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\044\000\011\000\013\000\012\000\012\000\013\000\011\000\
\\017\000\010\000\018\000\009\000\019\000\008\000\021\000\007\000\
\\023\000\006\000\024\000\005\000\025\000\004\000\026\000\003\000\
\\030\000\002\000\031\000\001\000\000\000\
\\001\000\045\000\011\000\013\000\012\000\012\000\013\000\011\000\
\\017\000\010\000\018\000\009\000\019\000\008\000\021\000\007\000\
\\023\000\006\000\024\000\005\000\025\000\004\000\026\000\003\000\
\\030\000\002\000\031\000\001\000\000\000\
\\001\000\046\000\011\000\013\000\012\000\012\000\013\000\011\000\
\\017\000\010\000\018\000\009\000\019\000\008\000\021\000\007\000\
\\023\000\006\000\024\000\005\000\025\000\004\000\026\000\003\000\
\\030\000\002\000\031\000\001\000\000\000\
\\001\000\048\000\011\000\013\000\012\000\012\000\013\000\011\000\
\\017\000\010\000\018\000\009\000\019\000\008\000\020\000\047\000\
\\021\000\007\000\023\000\006\000\024\000\005\000\025\000\004\000\
\\026\000\003\000\030\000\002\000\031\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\001\000\053\000\011\000\013\000\012\000\012\000\013\000\011\000\
\\017\000\010\000\018\000\009\000\019\000\008\000\021\000\007\000\
\\023\000\006\000\024\000\005\000\025\000\004\000\026\000\003\000\
\\030\000\002\000\031\000\001\000\000\000\
\\000\000\
\\001\000\055\000\011\000\013\000\012\000\012\000\013\000\011\000\
\\017\000\010\000\018\000\009\000\019\000\008\000\021\000\007\000\
\\023\000\006\000\024\000\005\000\025\000\004\000\026\000\003\000\
\\030\000\002\000\031\000\001\000\000\000\
\\001\000\056\000\011\000\013\000\012\000\012\000\013\000\011\000\
\\017\000\010\000\018\000\009\000\019\000\008\000\021\000\007\000\
\\023\000\006\000\024\000\005\000\025\000\004\000\026\000\003\000\
\\030\000\002\000\031\000\001\000\000\000\
\\001\000\057\000\011\000\013\000\012\000\012\000\013\000\011\000\
\\017\000\010\000\018\000\009\000\019\000\008\000\021\000\007\000\
\\023\000\006\000\024\000\005\000\025\000\004\000\026\000\003\000\
\\030\000\002\000\031\000\001\000\000\000\
\\001\000\058\000\011\000\013\000\012\000\012\000\013\000\011\000\
\\017\000\010\000\018\000\009\000\019\000\008\000\021\000\007\000\
\\023\000\006\000\024\000\005\000\025\000\004\000\026\000\003\000\
\\030\000\002\000\031\000\001\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\014\000\030\000\015\000\029\000\016\000\028\000\000\000\
\\014\000\030\000\015\000\029\000\016\000\028\000\000\000\
\\014\000\030\000\015\000\029\000\016\000\028\000\000\000\
\\000\000\
\\014\000\030\000\015\000\029\000\016\000\028\000\000\000\
\\000\000\
\\000\000\
\\001\000\065\000\011\000\013\000\012\000\012\000\013\000\011\000\
\\017\000\010\000\018\000\009\000\019\000\008\000\021\000\007\000\
\\023\000\006\000\024\000\005\000\025\000\004\000\026\000\003\000\
\\030\000\002\000\031\000\001\000\000\000\
\\001\000\067\000\011\000\013\000\012\000\012\000\013\000\011\000\
\\017\000\010\000\018\000\009\000\019\000\008\000\021\000\007\000\
\\023\000\006\000\024\000\005\000\025\000\004\000\026\000\003\000\
\\029\000\066\000\030\000\002\000\031\000\001\000\000\000\
\\014\000\030\000\015\000\029\000\016\000\028\000\000\000\
\\000\000\
\\014\000\030\000\015\000\029\000\016\000\028\000\000\000\
\\014\000\030\000\015\000\029\000\016\000\028\000\000\000\
\\014\000\030\000\015\000\029\000\016\000\028\000\000\000\
\\014\000\030\000\015\000\029\000\016\000\028\000\000\000\
\\001\000\069\000\011\000\013\000\012\000\012\000\013\000\011\000\
\\017\000\010\000\018\000\009\000\019\000\008\000\021\000\007\000\
\\023\000\006\000\024\000\005\000\025\000\004\000\026\000\003\000\
\\030\000\002\000\031\000\001\000\000\000\
\\001\000\070\000\011\000\013\000\012\000\012\000\013\000\011\000\
\\017\000\010\000\018\000\009\000\019\000\008\000\021\000\007\000\
\\023\000\006\000\024\000\005\000\025\000\004\000\026\000\003\000\
\\030\000\002\000\031\000\001\000\000\000\
\\001\000\071\000\011\000\013\000\012\000\012\000\013\000\011\000\
\\017\000\010\000\018\000\009\000\019\000\008\000\021\000\007\000\
\\023\000\006\000\024\000\005\000\025\000\004\000\026\000\003\000\
\\030\000\002\000\031\000\001\000\000\000\
\\000\000\
\\001\000\048\000\011\000\013\000\012\000\012\000\013\000\011\000\
\\017\000\010\000\018\000\009\000\019\000\008\000\020\000\072\000\
\\021\000\007\000\023\000\006\000\024\000\005\000\025\000\004\000\
\\026\000\003\000\030\000\002\000\031\000\001\000\000\000\
\\000\000\
\\014\000\030\000\015\000\029\000\016\000\028\000\000\000\
\\000\000\
\\014\000\030\000\015\000\029\000\016\000\028\000\000\000\
\\000\000\
\\014\000\030\000\015\000\029\000\016\000\028\000\000\000\
\\014\000\030\000\015\000\029\000\016\000\028\000\000\000\
\\014\000\030\000\015\000\029\000\016\000\028\000\000\000\
\\000\000\
\\001\000\079\000\011\000\013\000\012\000\012\000\013\000\011\000\
\\017\000\010\000\018\000\009\000\019\000\008\000\021\000\007\000\
\\023\000\006\000\024\000\005\000\025\000\004\000\026\000\003\000\
\\030\000\002\000\031\000\001\000\000\000\
\\000\000\
\\000\000\
\\001\000\067\000\011\000\013\000\012\000\012\000\013\000\011\000\
\\017\000\010\000\018\000\009\000\019\000\008\000\021\000\007\000\
\\023\000\006\000\024\000\005\000\025\000\004\000\026\000\003\000\
\\029\000\081\000\030\000\002\000\031\000\001\000\000\000\
\\001\000\082\000\011\000\013\000\012\000\012\000\013\000\011\000\
\\017\000\010\000\018\000\009\000\019\000\008\000\021\000\007\000\
\\023\000\006\000\024\000\005\000\025\000\004\000\026\000\003\000\
\\030\000\002\000\031\000\001\000\000\000\
\\001\000\083\000\011\000\013\000\012\000\012\000\013\000\011\000\
\\017\000\010\000\018\000\009\000\019\000\008\000\021\000\007\000\
\\023\000\006\000\024\000\005\000\025\000\004\000\026\000\003\000\
\\030\000\002\000\031\000\001\000\000\000\
\\014\000\030\000\015\000\029\000\016\000\028\000\022\000\084\000\000\000\
\\001\000\086\000\011\000\013\000\012\000\012\000\013\000\011\000\
\\017\000\010\000\018\000\009\000\019\000\008\000\021\000\007\000\
\\023\000\006\000\024\000\005\000\025\000\004\000\026\000\003\000\
\\030\000\002\000\031\000\001\000\000\000\
\\000\000\
\\014\000\030\000\015\000\029\000\016\000\028\000\000\000\
\\014\000\030\000\015\000\029\000\016\000\028\000\000\000\
\\000\000\
\\000\000\
\\014\000\030\000\015\000\029\000\016\000\028\000\000\000\
\\001\000\090\000\011\000\013\000\012\000\012\000\013\000\011\000\
\\017\000\010\000\018\000\009\000\019\000\008\000\021\000\007\000\
\\023\000\006\000\024\000\005\000\025\000\004\000\026\000\003\000\
\\030\000\002\000\031\000\001\000\000\000\
\\000\000\
\\000\000\
\\014\000\030\000\015\000\029\000\016\000\028\000\000\000\
\\001\000\092\000\011\000\013\000\012\000\012\000\013\000\011\000\
\\017\000\010\000\018\000\009\000\019\000\008\000\021\000\007\000\
\\023\000\006\000\024\000\005\000\025\000\004\000\026\000\003\000\
\\030\000\002\000\031\000\001\000\000\000\
\\014\000\030\000\015\000\029\000\016\000\028\000\022\000\093\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 95
val numrules = 73
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle General.Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(List.map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos = int
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit
 | STRING of unit ->  (string) | INT of unit ->  (int)
 | ID of unit ->  (string)
end
type svalue = MlyValue.svalue
type result = unit
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn (T 31) => true | (T 32) => true | (T 33) => true | (T 39) => true
 | (T 35) => true | (T 36) => true | (T 37) => true | (T 41) => true
 | (T 42) => true | (T 43) => true | (T 27) => true | (T 28) => true
 | (T 29) => true | (T 30) => true | (T 34) => true | (T 38) => true
 | (T 40) => true | _ => false
val preferred_change : (term list * term list) list = 
(nil
,nil
 $$ (T 29))::
(nil
,nil
 $$ (T 30))::
(nil
,nil
 $$ (T 7))::
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "ID"
  | (T 2) => "INT"
  | (T 3) => "STRING"
  | (T 4) => "COMMA"
  | (T 5) => "COLON"
  | (T 6) => "SEMICOLON"
  | (T 7) => "LPAREN"
  | (T 8) => "RPAREN"
  | (T 9) => "LBRACK"
  | (T 10) => "RBRACK"
  | (T 11) => "LBRACE"
  | (T 12) => "RBRACE"
  | (T 13) => "DOT"
  | (T 14) => "PLUS"
  | (T 15) => "MINUS"
  | (T 16) => "TIMES"
  | (T 17) => "DIVIDE"
  | (T 18) => "EQ"
  | (T 19) => "NEQ"
  | (T 20) => "LT"
  | (T 21) => "LE"
  | (T 22) => "GT"
  | (T 23) => "GE"
  | (T 24) => "AND"
  | (T 25) => "OR"
  | (T 26) => "ASSIGN"
  | (T 27) => "ARRAY"
  | (T 28) => "IF"
  | (T 29) => "THEN"
  | (T 30) => "ELSE"
  | (T 31) => "WHILE"
  | (T 32) => "FOR"
  | (T 33) => "TO"
  | (T 34) => "DO"
  | (T 35) => "LET"
  | (T 36) => "IN"
  | (T 37) => "END"
  | (T 38) => "OF"
  | (T 39) => "BREAK"
  | (T 40) => "NIL"
  | (T 41) => "FUNCTION"
  | (T 42) => "VAR"
  | (T 43) => "TYPE"
  | (T 44) => "UMINUS"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn (T 1) => MlyValue.ID(fn () => ("bogus")) | 
(T 2) => MlyValue.INT(fn () => (1)) | 
(T 3) => MlyValue.STRING(fn () => ("")) | 
_ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 44) $$ (T 43) $$ (T 42) $$ (T 41) $$ (T 40) $$ (T 39) $$ (T 38)
 $$ (T 37) $$ (T 36) $$ (T 35) $$ (T 34) $$ (T 33) $$ (T 32) $$ (T 31)
 $$ (T 30) $$ (T 29) $$ (T 28) $$ (T 27) $$ (T 26) $$ (T 25) $$ (T 24)
 $$ (T 23) $$ (T 22) $$ (T 21) $$ (T 20) $$ (T 19) $$ (T 18) $$ (T 17)
 $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10)
 $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.ntVOID exp1, exp1left, exp1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 1, ( result, exp1left, exp1right), rest671)
end
|  ( 1, ( ( _, ( _, NIL1left, NIL1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 0, ( result, NIL1left, NIL1right), rest671)
end
|  ( 2, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ntVOID expP1,
 _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result
 = MlyValue.ntVOID (fn _ => ( let val  expP1 = expP1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.ntVOID lvalue1, lvalue1left, lvalue1right))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 lvalue1 = lvalue1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, lvalue1left, lvalue1right), rest671)
end
|  ( 4, ( ( _, ( MlyValue.INT INT1, INT1left, INT1right)) :: rest671))
 => let val  result = MlyValue.ntVOID (fn _ => ( let val  INT1 = INT1
 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, INT1left, INT1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.STRING STRING1, STRING1left, STRING1right))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 STRING1 = STRING1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, STRING1left, STRING1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.ntVOID negation1, negation1left, 
negation1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  negation1 = negation1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, negation1left, negation1right), rest671)

end
|  ( 7, ( ( _, ( MlyValue.ntVOID arithmetic1, arithmetic1left, 
arithmetic1right)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  arithmetic1 = arithmetic1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, arithmetic1left, arithmetic1right), 
rest671)
end
|  ( 8, ( ( _, ( MlyValue.ntVOID comparison1, comparison1left, 
comparison1right)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  comparison1 = comparison1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, comparison1left, comparison1right), 
rest671)
end
|  ( 9, ( ( _, ( MlyValue.ntVOID boolcomp1, boolcomp1left, 
boolcomp1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  boolcomp1 = boolcomp1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, boolcomp1left, boolcomp1right), rest671)

end
|  ( 10, ( ( _, ( MlyValue.ntVOID record1, record1left, record1right))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 record1 = record1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, record1left, record1right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.ntVOID array1, array1left, array1right)) ::
 rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
array1 = array1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, array1left, array1right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.ntVOID assignment1, assignment1left, 
assignment1right)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  assignment1 = assignment1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, assignment1left, assignment1right), 
rest671)
end
|  ( 13, ( ( _, ( MlyValue.ntVOID ifThenElse1, ifThenElse1left, 
ifThenElse1right)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  ifThenElse1 = ifThenElse1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, ifThenElse1left, ifThenElse1right), 
rest671)
end
|  ( 14, ( ( _, ( MlyValue.ntVOID ifThen1, ifThen1left, ifThen1right))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 ifThen1 = ifThen1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, ifThen1left, ifThen1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.ntVOID while1, while1left, while1right)) ::
 rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
while1 = while1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, while1left, while1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.ntVOID forloop1, forloop1left, 
forloop1right)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  forloop1 = forloop1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, forloop1left, forloop1right), rest671)

end
|  ( 17, ( ( _, ( _, BREAK1left, BREAK1right)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 0, ( result, BREAK1left, BREAK1right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.ntVOID unit1, unit1left, unit1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
unit1 = unit1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, unit1left, unit1right), rest671)
end
|  ( 19, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.ntVOID args1
, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  args1 = args1 ()
 in ()
end; ()))
 in ( LrTable.NT 0, ( result, ID1left, RPAREN1right), rest671)
end
|  ( 20, ( ( _, ( MlyValue.ntVOID expP1, _, expP1right)) :: _ :: ( _, 
( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result
 = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  expP1 = expP1 ()
 in ()
end; ()))
 in ( LrTable.NT 19, ( result, exp1left, expP1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.ntVOID exp1, exp1left, exp1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 19, ( result, exp1left, exp1right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.ntVOID expP1, expP1left, expP1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
expP1 = expP1 ()
 in ()
end; ()))
 in ( LrTable.NT 27, ( result, expP1left, expP1right), rest671)
end
|  ( 23, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 27, ( result, defaultPos, defaultPos), rest671)
end
|  ( 24, ( ( _, ( MlyValue.ntVOID args1, _, args1right)) :: _ :: ( _, 
( MlyValue.ntVOID exp1, exp1left, _)) :: rest671)) => let val  result
 = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  args1 = args1 ()
 in ()
end; ()))
 in ( LrTable.NT 28, ( result, exp1left, args1right), rest671)
end
|  ( 25, ( ( _, ( MlyValue.ntVOID exp1, exp1left, exp1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 28, ( result, exp1left, exp1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: ( _, ( 
MlyValue.ntVOID op1, _, _)) :: ( _, ( MlyValue.ntVOID exp1, exp1left,
 _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let
 val  exp1 = exp1 ()
 val  op1 = op1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 17, ( result, exp1left, exp2right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: ( _, ( 
MlyValue.ntVOID comp1, _, _)) :: ( _, ( MlyValue.ntVOID exp1, exp1left
, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let
 val  exp1 = exp1 ()
 val  comp1 = comp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 16, ( result, exp1left, exp2right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: ( _, ( 
MlyValue.ntVOID bool1, _, _)) :: ( _, ( MlyValue.ntVOID exp1, exp1left
, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let
 val  exp1 = exp1 ()
 val  bool1 = bool1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 18, ( result, exp1left, exp2right), rest671)
end
|  ( 29, ( ( _, ( _, PLUS1left, PLUS1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 15, ( result, PLUS1left, PLUS1right), rest671)
end
|  ( 30, ( ( _, ( _, MINUS1left, MINUS1right)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 15, ( result, MINUS1left, MINUS1right), rest671)
end
|  ( 31, ( ( _, ( _, TIMES1left, TIMES1right)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 15, ( result, TIMES1left, TIMES1right), rest671)
end
|  ( 32, ( ( _, ( _, DIVIDE1left, DIVIDE1right)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 15, ( result, DIVIDE1left, DIVIDE1right), rest671)

end
|  ( 33, ( ( _, ( _, EQ1left, EQ1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 14, ( result, EQ1left, EQ1right), rest671)
end
|  ( 34, ( ( _, ( _, NEQ1left, NEQ1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 14, ( result, NEQ1left, NEQ1right), rest671)
end
|  ( 35, ( ( _, ( _, LT1left, LT1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 14, ( result, LT1left, LT1right), rest671)
end
|  ( 36, ( ( _, ( _, LE1left, LE1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 14, ( result, LE1left, LE1right), rest671)
end
|  ( 37, ( ( _, ( _, GT1left, GT1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 14, ( result, GT1left, GT1right), rest671)
end
|  ( 38, ( ( _, ( _, GE1left, GE1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 14, ( result, GE1left, GE1right), rest671)
end
|  ( 39, ( ( _, ( _, AND1left, AND1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 13, ( result, AND1left, AND1right), rest671)
end
|  ( 40, ( ( _, ( _, OR1left, OR1right)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 13, ( result, OR1left, OR1right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: ( _, ( _, 
MINUS1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 12, ( result, MINUS1left, exp1right), rest671)
end
|  ( 42, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( _, LPAREN1left, _))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 11, ( result, LPAREN1left, RPAREN1right), rest671)

end
|  ( 43, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.ntVOID recP1
, _, _)) :: ( _, ( MlyValue.ntVOID exp1, _, _)) :: _ :: ( _, ( 
MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  exp1 = exp1 ()
 val  recP1 = recP1 ()
 in ()
end; ()))
 in ( LrTable.NT 20, ( result, ID1left, RBRACE1right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.ntVOID recP1, _, recP1right)) :: ( _, ( 
MlyValue.ntVOID exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) ::
 ( _, ( _, COMMA1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  exp1 = exp1 ()
 val  recP1 = recP1 ()
 in ()
end; ()))
 in ( LrTable.NT 21, ( result, COMMA1left, recP1right), rest671)
end
|  ( 45, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 21, ( result, defaultPos, defaultPos), rest671)
end
|  ( 46, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: _ :: (
 _, ( MlyValue.ntVOID exp1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, 
ID1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  ID1 = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 22, ( result, ID1left, exp2right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.ntVOID decs1, _, decs1right)) :: ( _, ( 
MlyValue.ntVOID dec1, dec1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  dec1 = dec1 ()
 val  decs1 = decs1 ()
 in ()
end; ()))
 in ( LrTable.NT 2, ( result, dec1left, decs1right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.ntVOID dec1, dec1left, dec1right)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
dec1 = dec1 ()
 in ()
end; ()))
 in ( LrTable.NT 2, ( result, dec1left, dec1right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.ntVOID tydec1, tydec1left, tydec1right)) ::
 rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
tydec1 = tydec1 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, tydec1left, tydec1right), rest671)
end
|  ( 50, ( ( _, ( MlyValue.ntVOID vardec1, vardec1left, vardec1right))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 vardec1 = vardec1 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, vardec1left, vardec1right), rest671)
end
|  ( 51, ( ( _, ( MlyValue.ntVOID fundec1, fundec1left, fundec1right))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 fundec1 = fundec1 ()
 in ()
end; ()))
 in ( LrTable.NT 3, ( result, fundec1left, fundec1right), rest671)
end
|  ( 52, ( ( _, ( MlyValue.ntVOID ty1, _, ty1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, TYPE1left, _)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  ty1 = ty1 ()
 in ()
end; ()))
 in ( LrTable.NT 4, ( result, TYPE1left, ty1right), rest671)
end
|  ( 53, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 in ()
end; ()))
 in ( LrTable.NT 7, ( result, ID1left, ID1right), rest671)
end
|  ( 54, ( ( _, ( _, _, RBRACE1right)) :: ( _, ( MlyValue.ntVOID 
tyfields1, _, _)) :: ( _, ( _, LBRACE1left, _)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ( let val  tyfields1 = 
tyfields1 ()
 in ()
end; ()))
 in ( LrTable.NT 7, ( result, LBRACE1left, RBRACE1right), rest671)
end
|  ( 55, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( _, 
ARRAY1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn
 _ => ( let val  ID1 = ID1 ()
 in ()
end; ()))
 in ( LrTable.NT 7, ( result, ARRAY1left, ID1right), rest671)
end
|  ( 56, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 8, ( result, defaultPos, defaultPos), rest671)
end
|  ( 57, ( ( _, ( MlyValue.ntVOID tyfieldsP1, _, tyfieldsP1right)) :: 
( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left
, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let
 val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  tyfieldsP1 = tyfieldsP1 ()
 in ()
end; ()))
 in ( LrTable.NT 8, ( result, ID1left, tyfieldsP1right), rest671)
end
|  ( 58, ( rest671)) => let val  result = MlyValue.ntVOID (fn _ => ())
 in ( LrTable.NT 9, ( result, defaultPos, defaultPos), rest671)
end
|  ( 59, ( ( _, ( MlyValue.ntVOID tyfieldsP1, _, tyfieldsP1right)) :: 
( _, ( MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _))
 :: ( _, ( _, COMMA1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  tyfieldsP1 = tyfieldsP1 ()
 in ()
end; ()))
 in ( LrTable.NT 9, ( result, COMMA1left, tyfieldsP1right), rest671)

end
|  ( 60, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID1, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let
 val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 5, ( result, VAR1left, exp1right), rest671)
end
|  ( 61, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _,
 ( _, VAR1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  ID1 = ID1 ()
 val  ID2 = ID2 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 5, ( result, VAR1left, exp1right), rest671)
end
|  ( 62, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: _ :: (
 _, ( MlyValue.ntVOID tyfields1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1
, _, _)) :: ( _, ( _, FUNCTION1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 val  tyfields1 = tyfields1 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 6, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 63, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ID ID2, _, _)) :: _ :: _ :: ( _, ( MlyValue.ntVOID tyfields1,
 _, _)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, 
FUNCTION1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID
 (fn _ => ( let val  ID1 = ID1 ()
 val  tyfields1 = tyfields1 ()
 val  ID2 = ID2 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 6, ( result, FUNCTION1left, exp1right), rest671)
end
|  ( 64, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  ID1 = ID1 ()
 in ()
end; ()))
 in ( LrTable.NT 10, ( result, ID1left, ID1right), rest671)
end
|  ( 65, ( ( _, ( MlyValue.ID ID1, _, ID1right)) :: _ :: ( _, ( 
MlyValue.ntVOID lvalue1, lvalue1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  lvalue1 = lvalue1 ()
 val  ID1 = ID1 ()
 in ()
end; ()))
 in ( LrTable.NT 10, ( result, lvalue1left, ID1right), rest671)
end
|  ( 66, ( ( _, ( _, _, RBRACK1right)) :: ( _, ( MlyValue.ntVOID exp1,
 _, _)) :: _ :: ( _, ( MlyValue.ntVOID lvalue1, lvalue1left, _)) :: 
rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val  
lvalue1 = lvalue1 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 10, ( result, lvalue1left, RBRACK1right), rest671)

end
|  ( 67, ( ( _, ( MlyValue.ntVOID exp1, _, exp1right)) :: _ :: ( _, ( 
MlyValue.ntVOID lvalue1, lvalue1left, _)) :: rest671)) => let val  
result = MlyValue.ntVOID (fn _ => ( let val  lvalue1 = lvalue1 ()
 val  exp1 = exp1 ()
 in ()
end; ()))
 in ( LrTable.NT 23, ( result, lvalue1left, exp1right), rest671)
end
|  ( 68, ( ( _, ( MlyValue.ntVOID exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp2, _, _)) :: _ :: ( _, ( MlyValue.ntVOID exp1, _, _
)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = 
MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in ()
end; ()))
 in ( LrTable.NT 24, ( result, IF1left, exp3right), rest671)
end
|  ( 69, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) =>
 let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 25, ( result, IF1left, exp2right), rest671)
end
|  ( 70, ( ( _, ( MlyValue.ntVOID exp2, _, exp2right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp1, _, _)) :: ( _, ( _, WHILE1left, _)) :: rest671))
 => let val  result = MlyValue.ntVOID (fn _ => ( let val  exp1 = exp1
 ()
 val  exp2 = exp2 ()
 in ()
end; ()))
 in ( LrTable.NT 29, ( result, WHILE1left, exp2right), rest671)
end
|  ( 71, ( ( _, ( MlyValue.ntVOID exp3, _, exp3right)) :: _ :: ( _, ( 
MlyValue.ntVOID exp2, _, _)) :: _ :: ( _, ( MlyValue.ntVOID exp1, _, _
)) :: _ :: ( _, ( MlyValue.ID ID1, _, _)) :: ( _, ( _, FOR1left, _))
 :: rest671)) => let val  result = MlyValue.ntVOID (fn _ => ( let val 
 ID1 = ID1 ()
 val  exp1 = exp1 ()
 val  exp2 = exp2 ()
 val  exp3 = exp3 ()
 in ()
end; ()))
 in ( LrTable.NT 30, ( result, FOR1left, exp3right), rest671)
end
|  ( 72, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.ntVOID expseq1,
 _, _)) :: _ :: ( _, ( MlyValue.ntVOID decs1, _, _)) :: ( _, ( _, 
LET1left, _)) :: rest671)) => let val  result = MlyValue.ntVOID (fn _
 => ( let val  decs1 = decs1 ()
 val  expseq1 = expseq1 ()
 in ()
end; ()))
 in ( LrTable.NT 26, ( result, LET1left, END1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.ntVOID x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Tiger_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun INT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.INT (fn () => i),p1,p2))
fun STRING (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.STRING (fn () => i),p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun COLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun SEMICOLON (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun LBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun RBRACE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun PLUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun MINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
fun TIMES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(
ParserData.MlyValue.VOID,p1,p2))
fun DIVIDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(
ParserData.MlyValue.VOID,p1,p2))
fun EQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(
ParserData.MlyValue.VOID,p1,p2))
fun NEQ (p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(
ParserData.MlyValue.VOID,p1,p2))
fun LT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(
ParserData.MlyValue.VOID,p1,p2))
fun LE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(
ParserData.MlyValue.VOID,p1,p2))
fun GT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(
ParserData.MlyValue.VOID,p1,p2))
fun GE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(
ParserData.MlyValue.VOID,p1,p2))
fun ASSIGN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(
ParserData.MlyValue.VOID,p1,p2))
fun ARRAY (p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(
ParserData.MlyValue.VOID,p1,p2))
fun WHILE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(
ParserData.MlyValue.VOID,p1,p2))
fun FOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(
ParserData.MlyValue.VOID,p1,p2))
fun TO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(
ParserData.MlyValue.VOID,p1,p2))
fun DO (p1,p2) = Token.TOKEN (ParserData.LrTable.T 34,(
ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 35,(
ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 36,(
ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 37,(
ParserData.MlyValue.VOID,p1,p2))
fun OF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 38,(
ParserData.MlyValue.VOID,p1,p2))
fun BREAK (p1,p2) = Token.TOKEN (ParserData.LrTable.T 39,(
ParserData.MlyValue.VOID,p1,p2))
fun NIL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 40,(
ParserData.MlyValue.VOID,p1,p2))
fun FUNCTION (p1,p2) = Token.TOKEN (ParserData.LrTable.T 41,(
ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 42,(
ParserData.MlyValue.VOID,p1,p2))
fun TYPE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 43,(
ParserData.MlyValue.VOID,p1,p2))
fun UMINUS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 44,(
ParserData.MlyValue.VOID,p1,p2))
end
end
