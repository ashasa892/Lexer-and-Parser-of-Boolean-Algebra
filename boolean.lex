structure T = Tokens

	type pos = int
	type svalue = T.svalue
	type (’a,’b) token = (’a,’b) T.token
	type lexresult = (svalue, pos) token

	val lin = ref 1;
	val col = ref 0;

	fun error(tok, lin, col) = 
		TextIO.output(TextIO.stdOut, "Unknown token:"^Int.toString(lin)^":"^Int.toString(col)^": "^tok^"\n");

	fun eof() = T.EOF(!lin, !col);

%%




%% 

