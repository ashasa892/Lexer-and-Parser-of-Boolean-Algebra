structure BooleanLrVals = BooleanLrValsFun(structure Token = LrParser.Token)
structure BooleanLex = BooleanFun(structure Tokens = BooleanLrVals.Tokens);
structure BooleanParser =
	  Join(structure LrParser = LrParser
     	       structure ParserData = BooleanLrVals.ParserData
     	       structure Lex = BooleanLex)
     
fun invoke lexstream =
    	let fun print_error (s,line:int,col:int) =
		    TextIO.output(TextIO.stdOut, "SYNTAX ERROR:" ^ (Int.toString line) ^ ":" ^ (Int.toString col) ^ " " ^ s ^ "\n")
		in
		    BooleanParser.parse(0,lexstream,print_error,())
		end

fun stringToLexer fileName =
    let 
        val inStream = TextIO.openIn fileName;

    	val lexer = BooleanParser.makeLexer(fn n => if TextIO.endOfStream inStream
                                                     then ""
                                                     else TextIO.inputN (inStream,n))   
    in
    	lexer
    end	
		
fun parse (lexer) =
    let 
        val dummyEOF = BooleanLrVals.Tokens.EOF(0,0)
    	val (result, lexer) = invoke lexer
	    val (nextToken, lexer) = BooleanParser.Stream.get lexer
    in
        print("POSTORDER Traversal of Parse Tree:\n");
        if BooleanParser.sameToken(nextToken, dummyEOF) then result
 	    else (TextIO.output(TextIO.stdOut, "Warning: Unconsumed input \n"); result)
    end;


val parseString = parse o stringToLexer

fun compile(fileName) = print(parseString(fileName)) handle LexError s => print(s)
                                                        |   ParseError => print("");


