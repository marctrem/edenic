{
open Lexing
open Parser
open Core


exception SyntaxError of string

let ratio_of_string s =
  match String.split ~on:'/' s
  |> List.map ~f:int_of_string with
  | a :: b :: [] -> RATIO(a, b)
  | _ -> raise (SyntaxError (sprintf "Cannot parse ratio '%s'" s))

let base_num_of_string s =
  match String.split ~on:'r' s with
  | a :: b :: [] -> BASENUM(int_of_string a, b)
  | _ -> raise (SyntaxError (sprintf "Cannot parse base num '%s'" s))

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let int = '-'? ['0'-'9'] ['0'-'9']*

let hex_nibble = ['0'-'9' 'a'-'f']
let hex_digit = hex_nibble hex_nibble
let hex = hex_digit+

let backslash_char = ['A'-'Z' 'a'-'z' '0'-'9']

let digit = ['0'-'9']
let frac = '.' digit*
let exp = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? exp?

let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '-'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']*
let any_base_number = ['a'-'z' 'A'-'Z' '0'-'9' '=' '\\' '/' '+']+
let tag = '#' id


rule read =
  parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | "nil"    { NIL }
  | int      { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float    { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | "Inf"    { FLOAT (Float.infinity)}
  | "-Inf"   { FLOAT (Float.neg_infinity)}
  | "##NaN"  { FLOAT (Float.nan) }
  | '"'      { read_string (Buffer.create 17) lexbuf }
  | ':'      { read_keyword lexbuf }
  | '\\'     { read_char lexbuf }
  | '\''     { QUOTE }
  | '`'      { BACKTICK }
  | "true"   { TRUE }
  | "false"  { FALSE }
  | id       { SYMBOL (Lexing.lexeme lexbuf) }
  | '#'      { read_sharp lexbuf }
  | "("      { BEGIN_LIST }
  | ")"      { RIGHT_PAREN }
  | '{'      { BEGIN_ASSOC }
  | '}'      { RIGHT_BRACE }
  | '['      { BEGIN_VEC }
  | ']'      { RIGHT_BRACK }
  | int '/' int { ratio_of_string (Lexing.lexeme lexbuf) }
  | int 'r' any_base_number { base_num_of_string (Lexing.lexeme lexbuf) }
  | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }

and read_char =
  parse
  | "newline" { CHAR('\n') }
  | "return"  { CHAR('\r') }
  | "space"   { CHAR(' ') }
  | "tab"     { CHAR('\t') }
  | 'u'       { read_char_unicode lexbuf }
  | backslash_char # 'u' { CHAR(Lexing.lexeme_char lexbuf 0) }
  | (backslash_char # 'u') backslash_char* { raise (SyntaxError ("eInvalid char: \\" ^ Lexing.lexeme lexbuf)) }
  | _ { raise (SyntaxError (sprintf "Could not parse char literal '\\%s'." (Lexing.lexeme lexbuf))) }

and read_char_unicode =
  parse
  | "" { CHAR('u') }
  | hex_digit  { CHAR(Option.value_exn (Char.of_int (Int.of_string ("0x" ^ (Lexing.lexeme lexbuf))))) }
  | hex_digit hex_digit+ { raise (SyntaxError ("Characters wider than 1 byte are not supported at the moment: \\" ^ Lexing.lexeme lexbuf)) }
  | hex_digit backslash_char+ { raise (SyntaxError ("Invalid char: \\u" ^ Lexing.lexeme lexbuf)) }
  | backslash_char+ { raise (SyntaxError ("bleeeInvalid char: \\u" ^ Lexing.lexeme lexbuf)) }
  
and read_keyword =
  parse
  | id       { KEYWORD (Lexing.lexeme lexbuf) }

and read_sharp =
  parse
  | '{'      { BEGIN_SET }
  | '_'      { DISCARD }
  | id       { TAG (Lexing.lexeme lexbuf) }

(* part "5" *)
and read_string buf =
  parse
| '"'       { STRING (Buffer.contents buf) }
| '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
| '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
| '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
| '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
| '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
| '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
| '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
| [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
| _ { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
| eof { raise (SyntaxError ("String is not terminated")) }

