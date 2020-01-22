structure Mlex  = struct

    structure yyInput : sig

        type stream
	val mkStream : (int -> string) -> stream
	val fromStream : TextIO.StreamIO.instream -> stream
	val getc : stream -> (Char.char * stream) option
	val getpos : stream -> int
	val getlineNo : stream -> int
	val subtract : stream * stream -> string
	val eof : stream -> bool
	val lastWasNL : stream -> bool

      end = struct

        structure TIO = TextIO
        structure TSIO = TIO.StreamIO
	structure TPIO = TextPrimIO

        datatype stream = Stream of {
            strm : TSIO.instream,
	    id : int,  (* track which streams originated 
			* from the same stream *)
	    pos : int,
	    lineNo : int,
	    lastWasNL : bool
          }

	local
	  val next = ref 0
	in
	fun nextId() = !next before (next := !next + 1)
	end

	val initPos = 2 (* ml-lex bug compatibility *)

	fun mkStream inputN = let
              val strm = TSIO.mkInstream 
			   (TPIO.RD {
			        name = "lexgen",
				chunkSize = 4096,
				readVec = SOME inputN,
				readArr = NONE,
				readVecNB = NONE,
				readArrNB = NONE,
				block = NONE,
				canInput = NONE,
				avail = (fn () => NONE),
				getPos = NONE,
				setPos = NONE,
				endPos = NONE,
				verifyPos = NONE,
				close = (fn () => ()),
				ioDesc = NONE
			      }, "")
	      in 
		Stream {strm = strm, id = nextId(), pos = initPos, lineNo = 1,
			lastWasNL = true}
	      end

	fun fromStream strm = Stream {
		strm = strm, id = nextId(), pos = initPos, lineNo = 1, lastWasNL = true
	      }

	fun getc (Stream {strm, pos, id, lineNo, ...}) = (case TSIO.input1 strm
              of NONE => NONE
	       | SOME (c, strm') => 
		   SOME (c, Stream {
			        strm = strm', 
				pos = pos+1, 
				id = id,
				lineNo = lineNo + 
					 (if c = #"\n" then 1 else 0),
				lastWasNL = (c = #"\n")
			      })
	     (* end case*))

	fun getpos (Stream {pos, ...}) = pos

	fun getlineNo (Stream {lineNo, ...}) = lineNo

	fun subtract (new, old) = let
	      val Stream {strm = strm, pos = oldPos, id = oldId, ...} = old
	      val Stream {pos = newPos, id = newId, ...} = new
              val (diff, _) = if newId = oldId andalso newPos >= oldPos
			      then TSIO.inputN (strm, newPos - oldPos)
			      else raise Fail 
				"BUG: yyInput: attempted to subtract incompatible streams"
	      in 
		diff 
	      end

	fun eof s = not (isSome (getc s))

	fun lastWasNL (Stream {lastWasNL, ...}) = lastWasNL

      end

    datatype yystart_state = 
INITIAL
    structure UserDeclarations = 
      struct

type pos = int
type lexresult = unit

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = ()



      end

    datatype yymatch 
      = yyNO_MATCH
      | yyMATCH of yyInput.stream * action * yymatch
    withtype action = yyInput.stream * yymatch -> UserDeclarations.lexresult

    local

    val yytable = 
Vector.fromList []
    fun mk yyins = let
        (* current start state *)
        val yyss = ref INITIAL
	fun YYBEGIN ss = (yyss := ss)
	(* current input stream *)
        val yystrm = ref yyins
	(* get one char of input *)
	val yygetc = yyInput.getc
	(* create yytext *)
	fun yymktext(strm) = yyInput.subtract (strm, !yystrm)
        open UserDeclarations
        fun lex 
(yyarg as ()) = let 
     fun continue() = let
            val yylastwasn = yyInput.lastWasNL (!yystrm)
            fun yystuck (yyNO_MATCH) = raise Fail "stuck state"
	      | yystuck (yyMATCH (strm, action, old)) = 
		  action (strm, old)
	    val yypos = yyInput.getpos (!yystrm)
	    val yygetlineNo = yyInput.getlineNo
	    fun yyactsToMatches (strm, [],	  oldMatches) = oldMatches
	      | yyactsToMatches (strm, act::acts, oldMatches) = 
		  yyMATCH (strm, act, yyactsToMatches (strm, acts, oldMatches))
	    fun yygo actTable = 
		(fn (~1, _, oldMatches) => yystuck oldMatches
		  | (curState, strm, oldMatches) => let
		      val (transitions, finals') = Vector.sub (yytable, curState)
		      val finals = List.map (fn i => Vector.sub (actTable, i)) finals'
		      fun tryfinal() = 
		            yystuck (yyactsToMatches (strm, finals, oldMatches))
		      fun find (c, []) = NONE
			| find (c, (c1, c2, s)::ts) = 
		            if c1 <= c andalso c <= c2 then SOME s
			    else find (c, ts)
		      in case yygetc strm
			  of SOME(c, strm') => 
			       (case find (c, transitions)
				 of NONE => tryfinal()
				  | SOME n => 
				      yygo actTable
					(n, strm', 
					 yyactsToMatches (strm, finals, oldMatches)))
			   | NONE => tryfinal()
		      end)
	    in 
let
fun yyAction0 (strm, lastMatch : yymatch) = (yystrm := strm;
      (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue()))
fun yyAction1 (strm, lastMatch : yymatch) = (yystrm := strm; (continue()))
fun yyAction2 (strm, lastMatch : yymatch) = (yystrm := strm; (print "R1"))
fun yyAction3 (strm, lastMatch : yymatch) = (yystrm := strm; (print "R2"))
fun yyAction4 (strm, lastMatch : yymatch) = (yystrm := strm; (print "R3"))
fun yyAction5 (strm, lastMatch : yymatch) = let
      val yytext = yymktext(strm)
      in
        yystrm := strm;
        (ErrorMsg.error yypos ("illegal character " ^ yytext); continue())
      end
fun yyQ8 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ9 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"]"
              then yyQ9(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ7 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"]"
              then yyQ9(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ14 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ12 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\\"
              then yyAction4(strm, yyNO_MATCH)
            else if inp < #"\\"
              then if inp = #"5"
                  then yyAction4(strm, yyNO_MATCH)
                else if inp < #"5"
                  then if inp <= #"1"
                      then yyAction4(strm, yyNO_MATCH)
                      else yyQ12(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp = #"["
                  then yyQ12(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp = #"i"
              then yyAction4(strm, yyNO_MATCH)
            else if inp < #"i"
              then if inp = #"h"
                  then yyQ12(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp = #"|"
              then yyQ12(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ13 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\\"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #"\\"
              then if inp = #"5"
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < #"5"
                  then if inp <= #"1"
                      then yyAction2(strm, yyNO_MATCH)
                      else yyQ12(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp = #"["
                  then yyQ12(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"i"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #"i"
              then if inp = #"h"
                  then yyQ12(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"|"
              then yyQ12(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ10 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"2"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #"2"
              then if inp <= #"/"
                  then yyAction2(strm, yyNO_MATCH)
                  else yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp = #"["
              then yyQ14(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ11 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyQ14(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < #"["
              then if inp = #"1"
                  then yyQ11(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < #"1"
                  then if inp = #"0"
                      then yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyAction2(strm, yyNO_MATCH)
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"]"
              then yyQ14(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ6 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\\"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #"\\"
              then if inp = #"2"
                  then yyQ12(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < #"2"
                  then if inp = #"0"
                      then yyQ10(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                    else if inp = #"1"
                      then yyQ11(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyAction2(strm, yyNO_MATCH)
                else if inp = #"5"
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < #"5"
                  then yyQ12(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp = #"["
                  then yyQ13(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"i"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #"i"
              then if inp = #"^"
                  then yyAction2(strm, yyNO_MATCH)
                else if inp < #"^"
                  then yyQ14(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp = #"h"
                  then yyQ12(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"|"
              then yyQ12(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ21 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction3(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction3(strm, yyNO_MATCH)
      (* end case *))
fun yyQ20 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"\\"
              then yystuck(lastMatch)
            else if inp < #"\\"
              then if inp = #"1"
                  then yyQ20(strm', lastMatch)
                else if inp < #"1"
                  then if inp = #"0"
                      then yyQ19(strm', lastMatch)
                      else yystuck(lastMatch)
                else if inp = #"["
                  then yyQ19(strm', lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"|"
              then yyQ19(strm', lastMatch)
            else if inp < #"|"
              then if inp = #"]"
                  then yyQ21(strm', lastMatch)
                  else yystuck(lastMatch)
              else yystuck(lastMatch)
      (* end case *))
and yyQ19 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yystuck(lastMatch)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyQ19(strm', lastMatch)
            else if inp < #"["
              then if inp = #"1"
                  then yyQ20(strm', lastMatch)
                else if inp < #"1"
                  then if inp = #"0"
                      then yyQ19(strm', lastMatch)
                      else yystuck(lastMatch)
                  else yystuck(lastMatch)
            else if inp = #"|"
              then yyQ19(strm', lastMatch)
              else yystuck(lastMatch)
      (* end case *))
fun yyQ18 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyQ18(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp < #"["
              then if inp = #"1"
                  then yyQ20(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp < #"1"
                  then if inp = #"0"
                      then yyQ19(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                      else yyAction4(strm, yyNO_MATCH)
                else if inp <= #"4"
                  then yyQ12(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp = #"i"
              then yyAction4(strm, yyNO_MATCH)
            else if inp < #"i"
              then if inp = #"h"
                  then yyQ12(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp = #"|"
              then yyQ18(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ17 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyQ18(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < #"["
              then if inp = #"1"
                  then yyQ20(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < #"1"
                  then if inp = #"0"
                      then yyQ19(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyAction2(strm, yyNO_MATCH)
                else if inp <= #"4"
                  then yyQ12(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"i"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #"i"
              then if inp = #"h"
                  then yyQ12(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"|"
              then yyQ18(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ22 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyQ19(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < #"["
              then if inp = #"1"
                  then yyQ20(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < #"1"
                  then if inp = #"0"
                      then yyQ19(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyAction2(strm, yyNO_MATCH)
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"|"
              then yyQ19(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ16 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\\"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #"\\"
              then if inp = #"1"
                  then yyQ16(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < #"1"
                  then if inp = #"0"
                      then yyQ15(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyAction2(strm, yyNO_MATCH)
                else if inp = #"["
                  then yyQ22(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"|"
              then yyQ19(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < #"|"
              then if inp = #"]"
                  then yyQ21(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
and yyQ15 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyQ22(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < #"["
              then if inp = #"1"
                  then yyQ16(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < #"1"
                  then if inp = #"0"
                      then yyQ15(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyAction2(strm, yyNO_MATCH)
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"|"
              then yyQ19(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ5 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction2(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyQ17(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
            else if inp < #"["
              then if inp = #"1"
                  then yyQ16(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                else if inp < #"1"
                  then if inp = #"0"
                      then yyQ15(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                      else yyAction2(strm, yyNO_MATCH)
                else if inp <= #"4"
                  then yyQ12(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"i"
              then yyAction2(strm, yyNO_MATCH)
            else if inp < #"i"
              then if inp = #"h"
                  then yyQ12(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
                  else yyAction2(strm, yyNO_MATCH)
            else if inp = #"|"
              then yyQ18(strm', yyMATCH(strm, yyAction2, yyNO_MATCH))
              else yyAction2(strm, yyNO_MATCH)
      (* end case *))
fun yyQ4 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"["
              then yyQ18(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp < #"["
              then if inp = #"1"
                  then yyQ20(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp < #"1"
                  then if inp = #"0"
                      then yyQ19(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                      else yyAction4(strm, yyNO_MATCH)
                else if inp <= #"4"
                  then yyQ12(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp = #"i"
              then yyAction4(strm, yyNO_MATCH)
            else if inp < #"i"
              then if inp = #"h"
                  then yyQ12(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyAction4(strm, yyNO_MATCH)
            else if inp = #"|"
              then yyQ18(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyAction4(strm, yyNO_MATCH)
      (* end case *))
fun yyQ3 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction0(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction0(strm, yyNO_MATCH)
      (* end case *))
fun yyQ23 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction1(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ23(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ23(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ2 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction1(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"\n"
              then yyAction1(strm, yyNO_MATCH)
            else if inp < #"\n"
              then if inp = #"\t"
                  then yyQ23(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
                  else yyAction1(strm, yyNO_MATCH)
            else if inp = #" "
              then yyQ23(strm', yyMATCH(strm, yyAction1, yyNO_MATCH))
              else yyAction1(strm, yyNO_MATCH)
      (* end case *))
fun yyQ1 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE => yyAction5(strm, yyNO_MATCH)
        | SOME(inp, strm') => yyAction5(strm, yyNO_MATCH)
      (* end case *))
fun yyQ0 (strm, lastMatch : yymatch) = (case (yygetc(strm))
       of NONE =>
            if yyInput.eof(!(yystrm))
              then UserDeclarations.eof(yyarg)
              else yyAction4(strm, yyNO_MATCH)
        | SOME(inp, strm') =>
            if inp = #"0"
              then yyQ5(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp < #"0"
              then if inp = #" "
                  then yyQ2(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp < #" "
                  then if inp = #"\n"
                      then yyQ3(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                    else if inp < #"\n"
                      then if inp = #"\t"
                          then yyQ2(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                          else yyQ1(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                      else yyQ1(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp = #"*"
                  then yyQ4(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyQ1(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp = #"["
              then yyQ8(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp < #"["
              then if inp = #"5"
                  then yyQ7(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                else if inp < #"5"
                  then if inp = #"1"
                      then yyQ6(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                      else yyQ1(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
                  else yyQ1(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
            else if inp = #"]"
              then yyQ8(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
              else yyQ1(strm', yyMATCH(strm, yyAction4, yyNO_MATCH))
      (* end case *))
in
  (case (!(yyss))
   of INITIAL => yyQ0(!(yystrm), yyNO_MATCH)
  (* end case *))
end
            end
	  in 
            continue() 	  
	    handle IO.Io{cause, ...} => raise cause
          end
        in 
          lex 
        end
    in
    fun makeLexer yyinputN = mk (yyInput.mkStream yyinputN)
    end

  end
