{
  open Parser
  exception Eof
}

let int = ['0'-'9']['0'-'9']*
let var = ['a'-'z' 'A'-'Z' '_']['0'-'9' 'a'-'z' 'A'-'Z' '_']*
let white = [' ' '\t']
let newline = '\n' | '\r' | "\r\n"

rule token = parse
  | white { token lexbuf }
  | newline { EOL }
  | "let" { LET }
  | '=' { EQUALS }
  | "in" { IN }
  | "&&" { AND }
  | '^' { XOR }
  | "clean" { CLEAN }
  | "assert" { ASSERT }
  | "rotate" { ROTATE }
  | '.' { DOT }
  | "fun" { LAMBDA }
  | "<-" { LEFTARROW }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | int as lxm { NAT (int_of_string lxm) }
  | var as lxm { VAR lxm }
  | eof { raise Eof }
