{
module AnnB_Parser where
import AnnB_Lexer
import Types
}

%name parseAnnB
%tokentype { Token }
%error { parseError }

%token
  const         { TCONST _ $$ }
  var           { TVAR _ $$ }
  int           { TINT _ $$ }
  "("           { TOPENP _ }
  ")"           { TCLOSEP _ }
  "{"           { TOPENB _ }
  "}"           { TCLOSEB _ }
  ":"           { TCOLON _  }
  ","           { TCOMMA _ }
  "["           { TOPENSQB _ } 
  "]"           { TCLOSESQB _ }
  "<>"          { TDIAMOND _ }
  "*"           { TSTAR _ }
  ":="          { TASSIGN _ }
  "="           { TEQUAL _ }
  "/"           { TSLASH _ }
  "->"          { TCHANNEL _ }
  "if"          { TIF _ }
  "then"        { TTHEN _ }
  "else"        { TELSE _ }
  "end"         { TEND _ }
  "new"         { TNEW _ }
  "in"          { TIN _ }
  "dishonest"   { TDISHONEST _ }
  "private"     { TPRIVATE _ }
  "Protocol"    { TPROTOCOL _ }
  "Sigma0"      { TSIGMA0 _ }
  "Sigma"       { TSIGMA _ }
  "Cells"       { TCELLS _ }
  "Agents"      { TAGENTS _ }
  "Roles"       { TROLES _ }
  "Knowledge"   { TKNOWLEDGE _ }
  "Actions"	    { TACIONS _ }
  "Bound"       { TBOUND _ }
  "true"        { TTRUE _ }
  "false"       { TFALSE _ }
  "and"         { TAND _ }
  "not"         { TNOT _ }
  "or"          { TOR _ }

%%

annbspec :: {AnnB}
: "Protocol" ":" ident
  sigma0
  sigma
  "Agents" ":" agents
  "Roles" ":" roles
  "Knowledge" ":" knowledges
  cells
  "Actions" ":" actions
  "Bound" ":" int
  { ($3,$4,$5,$8,$11,$14,$15,$18,$21) }

sigma0 :: {[Def]}
: "Sigma0" ":" defs {$3}
| {[]}

sigma :: {[SigmaDef]}
: "Sigma" ":" sigmaDefs {$3}
| {[]}

cells :: {[CellDef]}
: "Cells" ":" cellDefs {$3}
| {[]}

defs :: {[Def]}
: def "," defs { $1:$3 }
| def {[$1]}

def :: {Def}
: const "/" int {($1,$3)}
| const {($1, 0)}

sigmaDefs :: {[SigmaDef]}
: defs sigmaDefs { (Public $1):$2 }
| "private" defs sigmaDefs { (Private $2):$3 }
| {[]}

consts :: {[String]}
: const "," consts {$1:$3}
| const {[$1]}

agents :: {[AgentDef]}
: consts agents { (Honest $1):$2 }
| "dishonest" consts agents { (Dishonest $2):$3 }
| {[]}

roles :: {[RoleDef]}
: var ":" consts roles {($1,$3):$4}
| {[]}

messages :: {[Msg]}
: message "," messages {$1:$3}
| message {[$1]}

message :: {Msg}
: const "(" messages ")" {Comp $1 $3}
| const {Atom $1}
| var {Atom $1}

knowledges :: {[Knowledge]}
: ident ":" messages knowledges { ($1,$3):$4}
| ident ":" messages {[($1,$3)]}

ident :: {String}
: const {$1}
| var {$1}

cellDefs :: {[CellDef]}
: const "[" message "]" ":=" message cellDefs { ($1, $3, $6):$7}
| const "[" message "]" ":=" message {[($1, $3, $6)]}

actions :: {Action}
: ident ":" process actions { (Local $1 $3 $4) }
| ident "->" ident ":" message actions { (Comm $1 $3 $5 $6) }
| {End}

vars :: {[Var]}
: var "," vars {$1:$3}
| var {[$1]}

mode :: {Mode}
: "*" {MStar}
| "<>" {MDiamond}

formula :: {Formula Msg}
: "true" {BTrue}
| "false" { (BNot BTrue) }
| formula "and" formula { (BAnd $1 $3) }
| formula "or" formula { (BOr $1 $3) }
| "not" formula { (BNot $2) }
| message "=" message { (BEq $1 $3) }
 
process :: {PProcess}
: "new" vars { (PNew $2) }
| var ":=" const "[" message "]" { (PRead $1 $3 $5) }
| const "[" message "]" ":=" message { (PWrite $1 $3 $6) }
| mode var "in" "{" consts "}" { (PChoice $1 $2 $5) }
| mode formula { (PRelease $1 $2) }
| "if" formula "then" actions "else" actions "end" { (PIf $2 $4 $6) }


------------

{

parseError :: [Token] -> a
parseError tks = error ("AnnB Parse error at " ++ lcn ++ "\n" )
	where
	lcn = case tks of
		  [] -> "end of file"
		  tk:_ -> "line " ++ show l ++ ", column " ++ show c ++ " - Token: " ++ show tk
			where
			AlexPn _ l c = tokenPosn tk

}
