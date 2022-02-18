%token <int> NAT
%token <string> VAR
%token <bool> BOOL
%token LPAREN
%token RPAREN
%token AND
%token XOR
%token CLEAN
%token ASSERT
%token APPEND
%token ROTATE
%token LEFTARROW
%token DOT
%token LET
%token EQUALS
%token IN
%token LAMBDA
%token EOL

%nonassoc DOT
%nonassoc IN
%nonassoc NAT
%left AND
%left XOR
%left LEFTARROW CLEAN ASSERT APPEND

%start <Expr.expr> prog

%%

prog:
  e = expr EOL { e }

expr:
  | n = NAT
    { Nat n }
  | v = VAR
    { Var v }
  | b = BOOL
    { Bool b }
  | LPAREN e = expr RPAREN
    { e }
  | e1 = expr AND e2 = expr
    { And (e1, e2) }
  | e1 = expr XOR e2 = expr
    { Xor (e1, e2) }
  | CLEAN e = expr
    { Clean e }
  | ASSERT e = expr
    { Assert e }
  | APPEND e1 = expr e2 = expr
    { Append (e1, e2) }
  | ROTATE i = NAT e = expr
    { Rotate (i, e) }
  | e1 = expr LEFTARROW e2 = expr
    { Assign (e1, e2) }
  | LAMBDA x = VAR DOT e = expr
    { Lambda (x, e) }
  | LET x = VAR EQUALS e1 = expr IN e2 = expr
    { Let (x, e1, e2) }
  
