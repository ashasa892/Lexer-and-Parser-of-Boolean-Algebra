structure T = Tokens

type pos = int
type svalue = T.svalue
type ('a,'b) token = ('a,'b) T.token
type lexresult = (svalue, pos) token

val lin = ref 1;
val col = ref 0;
val token_list = ref "[";
val eolpos = ref 1;

fun error(tok, lin, col) = 
	(raise LexError("Unknown token:"^Int.toString(lin)^":"^Int.toString(col)^": "^tok^"\n"))
	
	
fun eof() = (
				token_list := (!token_list) ^ "EOF]"; 
				print("LEXER OUTPUT: \n" ^ (!token_list) ^ "\n");
				T.EOF(!lin, !col)
			);

%%

%header (functor BooleanFun(structure Tokens: Boolean_TOKENS));


id = [A-Za-z]+;
space = [\ \t\b\r\f];

%% 

\n			=> (lin := (!lin)+1; eolpos := yypos+size yytext; lex());
{space}+	=> (lex());
"AND"		=> (token_list := (!token_list) ^ "AND \"" ^ yytext ^ "\","; col := yypos - (!eolpos); T.AND(!lin, !col));
"OR"		=> (token_list := (!token_list) ^ "OR \"" ^ yytext ^ "\","; col := yypos - (!eolpos); T.OR(!lin, !col));
"XOR"		=> (token_list := (!token_list) ^ "XOR \"" ^ yytext ^ "\","; col := yypos - (!eolpos); T.XOR(!lin, !col));
"EQUALS"	=> (token_list := (!token_list) ^ "EQUALS \"" ^ yytext ^ "\","; col := yypos - (!eolpos); T.EQUALS(!lin, !col));
"NOT"		=> (token_list := (!token_list) ^ "NOT \"" ^ yytext ^ "\","; col := yypos - (!eolpos); T.NOT(!lin, !col));
"IMPLIES"	=> (token_list := (!token_list) ^ "IMPLIES \"" ^ yytext ^ "\","; col := yypos - (!eolpos); T.IMPLIES(!lin, !col));
"IF"		=> (token_list := (!token_list) ^ "IF \"" ^ yytext ^ "\","; col := yypos - (!eolpos); T.IF(!lin, !col));
"THEN"		=> (token_list := (!token_list) ^ "THEN \"" ^ yytext ^ "\","; col := yypos - (!eolpos); T.THEN(!lin, !col));
"ELSE"		=> (token_list := (!token_list) ^ "ELSE \"" ^ yytext ^ "\","; col := yypos - (!eolpos); T.ELSE(!lin, !col));
TRUE		=> (token_list := (!token_list) ^ "TRUE \"" ^ yytext ^ "\","; col := yypos - (!eolpos); T.TRUE(!lin, !col));
"FALSE"		=> (token_list := (!token_list) ^ "FALSE \"" ^ yytext ^ "\","; col := yypos - (!eolpos); T.FALSE(!lin, !col));
"("			=> (token_list := (!token_list) ^ "LPAREN \"" ^ yytext ^ "\","; col := yypos - (!eolpos); T.LPAREN(!lin, !col));
")"			=> (token_list := (!token_list) ^ "RPAREN \"" ^ yytext ^ "\","; col := yypos - (!eolpos); T.RPAREN(!lin, !col));
";"			=> (token_list := (!token_list) ^ "TERM \"" ^ yytext ^ "\","; col := yypos - (!eolpos); T.TERM(!lin, !col));
{id}		=> (token_list := (!token_list) ^ "ID \"" ^ yytext ^ "\","; col := yypos - (!eolpos); T.ID(yytext, !lin, !col));
.			=> (col := yypos - (!eolpos); error(yytext, !lin, !col); lex());


