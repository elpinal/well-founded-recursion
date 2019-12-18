(* From mosml/examples/lexyacc/Main.sml. *)

fun parseExprReport file stream lexbuf =
    let val expr =
	    grammar.Main lexer.Token lexbuf
	    handle
	       Parsing.ParseError f =>
		   let val pos1 = Lexing.getLexemeStart lexbuf
		       val pos2 = Lexing.getLexemeEnd lexbuf
		   in
		       Location.errMsg (file, stream, lexbuf)
		                       (Location.Loc(pos1, pos2))
		                       "Syntax error."
		   end
	     | lexer.LexicalError(msg, pos1, pos2) =>
		   if pos1 >= 0 andalso pos2 >= 0 then
		       Location.errMsg (file, stream, lexbuf)
		                       (Location.Loc(pos1, pos2))
		                       ("Lexical error: " ^ msg)
		   else
		       (Location.errPrompt ("Lexical error: " ^ msg ^ "\n\n");
			raise Fail "Lexical error");
    in
	Parsing.clearParser();
	expr
    end
    handle exn => (Parsing.clearParser(); raise exn);

fun createLexerStream (is : BasicIO.instream) =
  Lexing.createLexer (fn buff => fn n => Nonstdio.buff_input is buff 0 n)

fun parse file =
    let val is     = Nonstdio.open_in_bin file
        val lexbuf = createLexerStream is
	val expr   = parseExprReport file is lexbuf
	             handle exn => (BasicIO.close_in is; raise exn)
    in
        BasicIO.close_in is;
	expr
    end

local
  val () = ignore (parse "examples/basic.wfr" |> Surface.Term.to_internal |> Semantics.type_of_closed)
    handle Semantics.Type.NotEqual(ty1, ty2) =>
      let open Pretty in
        "Type error: type mismatch:" <+> Syntax.Type.show ty1 <+> "vs" <+> Syntax.Type.show ty2
        |> print_endline
      end
in
end
