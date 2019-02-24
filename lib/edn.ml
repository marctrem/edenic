type value = [
    | `Nil
    | `Bool of bool
    | `Float of float
    | `Int of int
    | `Char of char
    | `List of value list
    | `Vec of value list
    | `Assoc of ((value * value) list)
    | `Set of value list
    | `String of string
    | `Keyword of string
    | `Symbol of string
    | `Tag of string * value
    | `Discard of value
    | `Quote of value
    | `Backtick of value
    | `Ratio of int * int
    | `BaseNum of int * string
]

open Core
open Out_channel


(*
let rec output_value outc = function
  | `Assoc obj -> print_assoc outc obj
  | `List l     -> print_list outc l
  | `String s   -> printf "\"%s\"" s
  | `Int i      -> printf "%d" i
  | `Float x    -> printf "%f" x
  | `Bool true  -> output_string outc "true"
  | `Bool false -> output_string outc "false"
  | `Null       -> output_string outc "null"
  | `Model _    -> output_string outc "somemodel"
*)

let rec output_value outc = function
  | `Nil -> output_string outc "nil"
  | `String s -> fprintf outc "\"%s\"" s
  | `Symbol s -> fprintf outc "%s" s
  | `Keyword s -> fprintf outc ":%s" s
  | `Tag (s, exp) -> fprintf outc "#%s " s; output_value outc exp
  | `Int i      -> fprintf outc "%d" i
  | `Float f    ->  (if Float.is_inf f then
                       (if f < 0.0 then output_char outc '-';
                        output_string outc "Inf")
                     else if Float.is_nan f then
                       output_string outc "##NaN"
                     else
                       fprintf outc "%f" f)
  | `Char c     -> print_char outc c
  | `Bool true  -> output_string outc "true"
  | `Bool false -> output_string outc "false"
  | `List li -> print_list outc li
  | `Vec vi -> print_vec outc vi
  | `Assoc av -> print_assoc outc av
  | `Set s -> print_set outc s
  | `Discard v -> fprintf outc "#_"; output_value outc v
  | `Quote v -> fprintf outc "'"; output_value outc v
  | `Backtick v -> fprintf outc "`"; output_value outc v
  | `Ratio (n, d) -> fprintf outc "%d/%d" n d
  | `BaseNum (b, r) -> fprintf outc "%dr%s" b r

and print_assoc outc obj =
  output_string outc "{";
  List.iter ~f:(fun (key, value) ->
      printf "%a %a" output_value key output_value value) 
    obj;
  output_string outc "}"

and print_list outc arr =
  output_string outc "(";
  List.iteri ~f:(fun i v ->
      if i > 0 then
        output_string outc " ";
      output_value outc v) arr;
  output_string outc ")"

and print_vec outc arr =
  output_string outc "[";
  List.iteri ~f:(fun i v ->
      if i > 0 then
        output_string outc " ";
      output_value outc v) arr;
  output_string outc "]"

and print_set outc s =
    output_string outc "#{";
  List.iteri ~f:(fun i v ->
      if i > 0 then
        output_string outc " ";
      output_value outc v) s;
  output_string outc "}"

and char_to_out = function
  | '\n' -> "\\newline"
  | '\r' -> "\\return"
  | ' ' -> "\\space"
  | '\t' -> "\\tab"
  | c -> sprintf "\\%c" c
  
and print_char outc c =
  output_string outc (char_to_out c)
