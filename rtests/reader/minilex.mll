{ }
rule nextchar = parse 
    _ 
      { Some (Lexing.lexeme lexbuf).[0] }
  | eof
      { None }
{ }
