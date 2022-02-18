open Revs

let prompt () = print_string ">> "; flush stdout

let () =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      prompt ();
      let expr = Parser.prog Lexer.token lexbuf in
      print_endline (Expr.string_of_expr expr);
      flush stdout;
    done
  with
    Lexer.Eof -> exit 0 
