functor BooleanLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Boolean_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(* User declarations *)


end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\000\000\000\000\
\\001\000\002\000\011\000\004\000\010\000\005\000\009\000\006\000\008\000\
\\008\000\007\000\014\000\006\000\000\000\
\\001\000\003\000\028\000\009\000\016\000\010\000\015\000\011\000\014\000\
\\012\000\013\000\013\000\012\000\000\000\
\\001\000\007\000\017\000\009\000\016\000\010\000\015\000\011\000\014\000\
\\012\000\013\000\013\000\012\000\000\000\
\\001\000\009\000\016\000\010\000\015\000\011\000\014\000\012\000\013\000\
\\013\000\012\000\015\000\027\000\000\000\
\\001\000\009\000\016\000\010\000\015\000\011\000\014\000\012\000\013\000\
\\013\000\012\000\016\000\030\000\000\000\
\\033\000\000\000\
\\034\000\000\000\
\\035\000\002\000\011\000\004\000\010\000\005\000\009\000\006\000\008\000\
\\008\000\007\000\014\000\006\000\000\000\
\\036\000\000\000\
\\037\000\000\000\
\\038\000\000\000\
\\039\000\000\000\
\\040\000\000\000\
\\041\000\000\000\
\\042\000\000\000\
\\043\000\009\000\016\000\010\000\015\000\011\000\014\000\012\000\013\000\
\\013\000\012\000\000\000\
\\044\000\009\000\016\000\010\000\015\000\011\000\014\000\012\000\013\000\
\\013\000\012\000\000\000\
\\045\000\000\000\
\\046\000\000\000\
\\047\000\000\000\
\"
val actionRowNumbers =
"\001\000\003\000\008\000\006\000\
\\001\000\001\000\019\000\018\000\
\\020\000\001\000\001\000\001\000\
\\001\000\001\000\001\000\009\000\
\\007\000\004\000\011\000\002\000\
\\016\000\015\000\014\000\013\000\
\\012\000\001\000\010\000\005\000\
\\001\000\017\000\000\000"
val gotoT =
"\
\\001\000\003\000\002\000\002\000\003\000\001\000\004\000\030\000\000\000\
\\000\000\
\\001\000\016\000\002\000\002\000\003\000\001\000\000\000\
\\000\000\
\\003\000\017\000\000\000\
\\003\000\018\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\003\000\019\000\000\000\
\\003\000\020\000\000\000\
\\003\000\021\000\000\000\
\\003\000\022\000\000\000\
\\003\000\023\000\000\000\
\\003\000\024\000\000\000\
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
\\003\000\027\000\000\000\
\\000\000\
\\000\000\
\\003\000\029\000\000\000\
\\000\000\
\\000\000\
\"
val numstates = 31
val numrules = 15
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
 | ID of unit ->  (string) | START of unit ->  (string)
 | FORMULA of unit ->  (string) | STATEMENT of unit ->  (string)
 | PROGRAM of unit ->  (string)
end
type svalue = MlyValue.svalue
type result = string
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 0) => true | _ => false
val showTerminal =
fn (T 0) => "EOF"
  | (T 1) => "LPAREN"
  | (T 2) => "RPAREN"
  | (T 3) => "ID"
  | (T 4) => "TRUE"
  | (T 5) => "FALSE"
  | (T 6) => "TERM"
  | (T 7) => "NOT"
  | (T 8) => "AND"
  | (T 9) => "OR"
  | (T 10) => "XOR"
  | (T 11) => "EQUALS"
  | (T 12) => "IMPLIES"
  | (T 13) => "IF"
  | (T 14) => "THEN"
  | (T 15) => "ELSE"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9)
 $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 2) $$ (T 1) $$ (T 
0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.PROGRAM PROGRAM1, PROGRAM1left, 
PROGRAM1right)) :: rest671)) => let val  result = MlyValue.START (fn _
 => let val  (PROGRAM as PROGRAM1) = PROGRAM1 ()
 in (PROGRAM1 ^ "," ^ "\"RULE: START -> PROGRAM\"\n")
end)
 in ( LrTable.NT 3, ( result, PROGRAM1left, PROGRAM1right), rest671)

end
|  ( 1, ( ( _, ( MlyValue.PROGRAM PROGRAM1, _, PROGRAM1right)) :: ( _,
 ( MlyValue.STATEMENT STATEMENT1, STATEMENT1left, _)) :: rest671)) =>
 let val  result = MlyValue.PROGRAM (fn _ => let val  (STATEMENT as 
STATEMENT1) = STATEMENT1 ()
 val  (PROGRAM as PROGRAM1) = PROGRAM1 ()
 in (
STATEMENT1 ^ "," ^ PROGRAM1 ^ "," ^ "\"RULE: PROGRAM -> STATEMENT PROGRAM\""
)
end)
 in ( LrTable.NT 0, ( result, STATEMENT1left, PROGRAM1right), rest671)

end
|  ( 2, ( ( _, ( MlyValue.STATEMENT STATEMENT1, STATEMENT1left, 
STATEMENT1right)) :: rest671)) => let val  result = MlyValue.PROGRAM
 (fn _ => let val  (STATEMENT as STATEMENT1) = STATEMENT1 ()
 in (STATEMENT1 ^ "," ^ "\"RULE: PROGRAM -> STATEMENT\"")
end)
 in ( LrTable.NT 0, ( result, STATEMENT1left, STATEMENT1right), 
rest671)
end
|  ( 3, ( ( _, ( _, _, TERM1right)) :: ( _, ( MlyValue.FORMULA 
FORMULA1, FORMULA1left, _)) :: rest671)) => let val  result = 
MlyValue.STATEMENT (fn _ => let val  (FORMULA as FORMULA1) = FORMULA1
 ()
 in (
FORMULA1 ^ "," ^ "TERM \";\"," ^ "\"RULE: STATEMENT -> FORMULA TERM\""
)
end)
 in ( LrTable.NT 1, ( result, FORMULA1left, TERM1right), rest671)
end
|  ( 4, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.FORMULA 
FORMULA1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let
 val  result = MlyValue.FORMULA (fn _ => let val  (FORMULA as FORMULA1
) = FORMULA1 ()
 in (
"LPAREN \"(\"," ^ FORMULA1 ^ ",RPAREN \")\"," ^ "\"RULE: FORMULA -> LPAREN FORMULA RPAREN\""
)
end)
 in ( LrTable.NT 2, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 5, ( ( _, ( MlyValue.FORMULA FORMULA1, _, FORMULA1right)) :: ( _,
 ( _, NOT1left, _)) :: rest671)) => let val  result = MlyValue.FORMULA
 (fn _ => let val  (FORMULA as FORMULA1) = FORMULA1 ()
 in ("NOT \"NOT\"," ^ FORMULA1 ^ ",\"RULE: FORMULA -> NOT FORMULA\"")

end)
 in ( LrTable.NT 2, ( result, NOT1left, FORMULA1right), rest671)
end
|  ( 6, ( ( _, ( MlyValue.FORMULA FORMULA2, _, FORMULA2right)) :: _ ::
 ( _, ( MlyValue.FORMULA FORMULA1, FORMULA1left, _)) :: rest671)) =>
 let val  result = MlyValue.FORMULA (fn _ => let val  (FORMULA as 
FORMULA1) = FORMULA1 ()
 val  FORMULA2 = FORMULA2 ()
 in (
FORMULA1 ^ ",AND \"AND\"," ^ FORMULA2 ^ ",\"RULE: FORMULA -> FORMULA AND FORMULA\""
)
end)
 in ( LrTable.NT 2, ( result, FORMULA1left, FORMULA2right), rest671)

end
|  ( 7, ( ( _, ( MlyValue.FORMULA FORMULA2, _, FORMULA2right)) :: _ ::
 ( _, ( MlyValue.FORMULA FORMULA1, FORMULA1left, _)) :: rest671)) =>
 let val  result = MlyValue.FORMULA (fn _ => let val  (FORMULA as 
FORMULA1) = FORMULA1 ()
 val  FORMULA2 = FORMULA2 ()
 in (
FORMULA1 ^ ",OR \"OR\"," ^ FORMULA2 ^ ",\"RULE: FORMULA -> FORMULA OR FORMULA\""
)
end)
 in ( LrTable.NT 2, ( result, FORMULA1left, FORMULA2right), rest671)

end
|  ( 8, ( ( _, ( MlyValue.FORMULA FORMULA2, _, FORMULA2right)) :: _ ::
 ( _, ( MlyValue.FORMULA FORMULA1, FORMULA1left, _)) :: rest671)) =>
 let val  result = MlyValue.FORMULA (fn _ => let val  (FORMULA as 
FORMULA1) = FORMULA1 ()
 val  FORMULA2 = FORMULA2 ()
 in (
FORMULA1 ^ ",XOR \"XOR\"," ^ FORMULA2 ^ ",\"RULE: FORMULA -> FORMULA XOR FORMULA\""
)
end)
 in ( LrTable.NT 2, ( result, FORMULA1left, FORMULA2right), rest671)

end
|  ( 9, ( ( _, ( MlyValue.FORMULA FORMULA2, _, FORMULA2right)) :: _ ::
 ( _, ( MlyValue.FORMULA FORMULA1, FORMULA1left, _)) :: rest671)) =>
 let val  result = MlyValue.FORMULA (fn _ => let val  (FORMULA as 
FORMULA1) = FORMULA1 ()
 val  FORMULA2 = FORMULA2 ()
 in (
FORMULA1 ^ ",EQUALS \"EQUALS\"," ^ FORMULA2 ^ ",\"RULE: FORMULA -> FORMULA EQUALS FORMULA\""
)
end)
 in ( LrTable.NT 2, ( result, FORMULA1left, FORMULA2right), rest671)

end
|  ( 10, ( ( _, ( MlyValue.FORMULA FORMULA2, _, FORMULA2right)) :: _
 :: ( _, ( MlyValue.FORMULA FORMULA1, FORMULA1left, _)) :: rest671))
 => let val  result = MlyValue.FORMULA (fn _ => let val  (FORMULA as 
FORMULA1) = FORMULA1 ()
 val  FORMULA2 = FORMULA2 ()
 in (
FORMULA1 ^ ",IMPLIES \"IMPLIES\"," ^ FORMULA2 ^ ",\"RULE: FORMULA -> FORMULA IMPLIES FORMULA\""
)
end)
 in ( LrTable.NT 2, ( result, FORMULA1left, FORMULA2right), rest671)

end
|  ( 11, ( ( _, ( MlyValue.FORMULA FORMULA3, _, FORMULA3right)) :: _
 :: ( _, ( MlyValue.FORMULA FORMULA2, _, _)) :: _ :: ( _, ( 
MlyValue.FORMULA FORMULA1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671
)) => let val  result = MlyValue.FORMULA (fn _ => let val  (FORMULA
 as FORMULA1) = FORMULA1 ()
 val  FORMULA2 = FORMULA2 ()
 val  FORMULA3 = FORMULA3 ()
 in (
"IF \"IF\"," ^ FORMULA1 ^ ",THEN \"THEN\"," ^ FORMULA2 ^ ",ELSE \"ELSE\"," ^ FORMULA3 ^  
												",\"RULE: FORMULA -> IF FORMULA THEN FORMULA ELSE FORMULA\""
)
end)
 in ( LrTable.NT 2, ( result, IF1left, FORMULA3right), rest671)
end
|  ( 12, ( ( _, ( _, TRUE1left, TRUE1right)) :: rest671)) => let val  
result = MlyValue.FORMULA (fn _ => (
"CONST \"TRUE\"," ^ "\"RULE: FORMULA -> TRUE\""))
 in ( LrTable.NT 2, ( result, TRUE1left, TRUE1right), rest671)
end
|  ( 13, ( ( _, ( _, FALSE1left, FALSE1right)) :: rest671)) => let
 val  result = MlyValue.FORMULA (fn _ => (
"CONST \"FALSE\"," ^ "\"RULE: FORMULA -> FALSE\""))
 in ( LrTable.NT 2, ( result, FALSE1left, FALSE1right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) =>
 let val  result = MlyValue.FORMULA (fn _ => let val  (ID as ID1) = 
ID1 ()
 in ("ID \"" ^ ID1 ^ "\"," ^ "\"RULE: FORMULA -> ID\"")
end)
 in ( LrTable.NT 2, ( result, ID1left, ID1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.START x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Boolean_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(
ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(
ParserData.MlyValue.VOID,p1,p2))
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(
ParserData.MlyValue.VOID,p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(
ParserData.MlyValue.ID (fn () => i),p1,p2))
fun TRUE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(
ParserData.MlyValue.VOID,p1,p2))
fun FALSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(
ParserData.MlyValue.VOID,p1,p2))
fun TERM (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(
ParserData.MlyValue.VOID,p1,p2))
fun NOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(
ParserData.MlyValue.VOID,p1,p2))
fun AND (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(
ParserData.MlyValue.VOID,p1,p2))
fun OR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(
ParserData.MlyValue.VOID,p1,p2))
fun XOR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(
ParserData.MlyValue.VOID,p1,p2))
fun EQUALS (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(
ParserData.MlyValue.VOID,p1,p2))
fun IMPLIES (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(
ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(
ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(
ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(
ParserData.MlyValue.VOID,p1,p2))
end
end
