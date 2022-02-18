open Printf

type expr =
  | Nat of int
  | Bool of bool
  | Var of string
  | Let of string * expr * expr
  | And of expr * expr
  | Xor of expr * expr
  | Clean of expr
  | Assert of expr
  | Append of expr * expr
  | Rotate of int * expr
  | Assign of expr * expr
  | App of expr * expr
  | Lambda of string * expr


let rec string_of_expr e =
  match e with
  | Nat n -> string_of_int n
  | Var v -> v
  | Let (binder, e1, e2) ->
      sprintf "(let %s = %s in %s)"
        binder
        (string_of_expr e1)
        (string_of_expr e2)
  | And (e1, e2) -> 
      sprintf "(%s && %s)"
        (string_of_expr e1)
        (string_of_expr e2)
  | Xor (e1, e2) -> 
      sprintf "(%s ^ %s)"
        (string_of_expr e1)
        (string_of_expr e2)
  | Clean e ->
      sprintf "(clean %s)" (string_of_expr e)
  | Assert e ->
      sprintf "(assert %s)" (string_of_expr e)
  | Append (e1, e2) ->
      sprintf "(append %s %s)"
        (string_of_expr e1)
        (string_of_expr e2)
  | Rotate (i, e) ->
      sprintf "(rotate %d %s)"
        i
        (string_of_expr e)
  | Assign (e1, e2) ->
      sprintf "(%s <- %s)"
        (string_of_expr e1) 
        (string_of_expr e2)
  | Lambda (x, e) ->
      sprintf "(fun %s. %s)"
        x
        (string_of_expr e)
  | _ -> "not implemented"
