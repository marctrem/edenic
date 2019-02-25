open Core
(* open Lexing *)
(*  Lexer = Olive__Lexer
 * module Parser = Olive__Parser *)

let eval (s:string) : Edn.value option =
  let lexbuf = Lexing.from_string s in
  Parser.prog Lexer.read lexbuf

let%test _ = eval "" = None
let%test _ = eval "hello" = Some(`Symbol "hello")
let%test _ = eval "\"hello\"" = Some(`String "hello")
let%test _ = eval ":hello" = Some(`Keyword "hello")
let%test _ = eval ":hello" = Some(`Keyword "hello")
let%test _ = eval "3/4" = Some(`Ratio (3, 4))
let%test _ = eval "16rdeadbeef" = Some(`BaseNum (16, "deadbeef"))
let%test _ = eval "#car {:color \"blue\"}" = 
               Some(`Tag ("car", 
                          `Assoc [(`Keyword "color", `String "blue")]))

let%test _ = eval "nil" = Some(`Nil)
let%test _ = eval "true" = Some(`Bool true)
let%test _ = eval "false" = Some(`Bool false)
let%test _ = eval "42" = Some(`Int 42)
let%test _ = eval "4.2" = Some(`Float 4.2)
let%test _ = eval "\\newline" = Some(`Char '\n')
let%test _ = eval "\\u42" = Some(`Char ((Char.of_int_exn 0x42)))
let%test _ = eval "`32" = Some(`Backtick (`Int 32))
let%test _ = eval "(1 2 3)" = Some(`List (`Int 1 :: `Int 2 :: `Int 3 :: []))
let%test _ = eval "#{1 2 3}" = Some(`Set (`Int 1 :: `Int 2 :: `Int 3 :: []))
let%test _ = eval "10r32" = Some(`BaseNum(10, "32"))
let%test _ = eval "64raGVsbG8hCg==" = Some(`BaseNum(64, "aGVsbG8hCg=="))
