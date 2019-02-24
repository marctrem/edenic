

let of_string (s:string) : Edn.value option =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.read lexbuf


