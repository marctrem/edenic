%token <int> INT
%token <float> FLOAT

%token <string> SYMBOL
%token <string> STRING
%token <string> KEYWORD
%token <string> TAG
%token <int * int> RATIO
%token <int * string> BASENUM

%token <char> CHAR

%token TRUE
%token FALSE

%token QUOTE
%token BACKTICK

%token NIL

// brace {
%token BEGIN_ASSOC
%token BEGIN_SET
%token RIGHT_BRACE

// brack [
%token BEGIN_VECTOR
%token RIGHT_BRACK

// PAREN (
%token BEGIN_LIST
%token RIGHT_PAREN

%token DISCARD

%token EOF

// Comments
%token <string> PREFIX_COMMENT
%token <string> SUFFIX_COMMENT

%start <Edn.value option> prog
%%

prog:
  | EOF { None }
  | v = value { Some v }
;

value:
  | NIL         { `Nil }
  | TRUE        { `Bool true }
  | FALSE       { `Bool false }
  | i = INT     { `Int i }
  | f = FLOAT   { `Float f }
  | r = RATIO   { `Ratio r }
  | b = BASENUM { `BaseNum b }
  | c = CHAR    { `Char c }
  | s = SYMBOL  { `Symbol s }
  | s = STRING  { `String s }
  | k = KEYWORD { `Keyword k }
  | t = TAG; exp = value     { `Tag (t, exp) }
  | lv = edn_list { `List lv }
  | av = edn_assoc { `Assoc av }
  | s = edn_set { `Set s }
  | DISCARD; exp = value { `Discard exp }
  | QUOTE; exp = value  { `Quote exp }
  | BACKTICK; exp = value { `Backtick exp }
;

edn_list:
  | BEGIN_LIST; list_values = value*; RIGHT_PAREN { list_values }
;

edn_assoc:
  | BEGIN_ASSOC; lp = list(pair(value, value)); RIGHT_BRACE { lp }
;

edn_set:
  | BEGIN_SET; s = value*; RIGHT_BRACE { s }
;
