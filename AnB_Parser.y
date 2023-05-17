{
module AnBParser where
}

%name parseAnB
%tokentype { Token }
%error { parseError }

%token
  ident         { TATOM _ $$ }
  int           { TINT _ $$ }
  "{"           { TBRACL _ }
  "}"           { TBRACR _ }
  "("           { TPARL _ }
  ")"           { TPARR _ }
  ","           { TCOMMA _ }
  "/"           { TSLASH _ }
  ":"           { TCOL _ }
  ";"           { TSEMICOL _ }
  "->"          { TARROW _ }
  "new"         { TNEW _ }
  "Sigma0"      { TSIGMA0 _ }
  "Sigma"       { TSIGMA _ }
  "Algebra"     { TALGEBRA _ }
  "Actions"     { TACTIONS _ }
  "Bound"       { TBOUND _ }
  "Agent"       { TAGENT _ }
  "Function"    { TFUNC _ }
  "Number"      { TNUM _ } -- maybe rename to Variable? or Var
  "private"     { TPRIVATE _ }

%%

anbspec :: {AST}
: declarations algebra actions bound

bound :: {Int}
: "Bound" ":" int ";" {$3}
| {0}

algebra :: {[(Message,Message)]}
: "Algebra" ":" rewriterules {$3}
| {[]}

rewriterules :: {[(Message,Message)]}
: message "->" message ";" rewriterules {($1,$3):$5}
| {[]}

messages :: {[Message]}
: message "," messages {$1:$3}
| message {[$1]}

message :: {Message}
: ident "(" messages ")" {Comp $1 $3}
| ident {Var $1}

declarations :: {[Declaration]}
: "Sigma0" ":" decs declarations {(Sigma0 $3)++$4}
| "Sigma" ":" decs declarations {$3++$4}
| {[]}

decs :: {[Declaration]}
: decorator "Agent" agents ";" decs { (map (\ ()) $3)++$5}
| decorator "Function" functions ";" decs {$3++$5}
| decorator "Number" numbers ";" decs {$3++$5}
| {[]}

decorator :: {Bool}
: "private" { True }
| { False }

agents :: {[Declaration]}
: agent "," agents {$1:$3}
| agent {[$1]}

agent :: {Declaration}
: ident { Agent $1 }
| ident "in" "{" idents "}" { AgentDomain $1,$4 }

idents :: {[PId]}
: ident "," idents {$1:$3}
| ident {[$1]}

functions :: {[Declaration]}
: funarity "," functions {$1:$3}
| funarity {[$1]}

funarity :: {Declaration}
: ident "/" int {(Fun $3,$1)}

numbers :: {[Declaration]}
: idents { (map (\ x -> Num x) $1) }

actions :: {[Action]}
: ident ":" process actions { (ProcessAction $3,$1):$4 }
| ident "->" ident ":" message actions { (MessageAction $5,$1,$3):$6 }
| {[]}

process :: {PProcess} -- maybe a better name
: "new" ident { (New $2) }

------------

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
