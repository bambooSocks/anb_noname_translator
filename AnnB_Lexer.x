{
module AnnB_Lexer where
}

%wrapper "posn"

$digit   = [0-9]
$alpha   = [A-Za-z]
$alphaL  = [a-z]
$alphaU  = [A-Z]
$identChar   = [a-zA-Z0-9_]

tokens :-
  $white+             ;
  "#".*               ;
  "("                 { (\ p s -> TOPENP p) }
  ")"                 { (\ p s -> TCLOSEP p) }
  "{"                 { (\ p s -> TOPENB p) }
  "}"                 { (\ p s -> TCLOSEB p) }
  ":"                 { (\ p s -> TCOLON p)  }
  ","                 { (\ p s -> TCOMMA p) }
  "["                 { (\ p s -> TOPENSQB p) } 
  "]"                 { (\ p s -> TCLOSESQB p) }
  "<>"                { (\ p s -> TDIAMOND p) }
  "*"                 { (\ p s -> TSTAR p) }
  ":="                { (\ p s -> TASSIGN p) }
  "="                 { (\ p s -> TEQUAL p) }
  "/"                 { (\ p s -> TSLASH p) }
  "->"                { (\ p s -> TCHANNEL p) }
  "if"                { (\ p s -> TIF p) }
  "then"              { (\ p s -> TTHEN p) }
  "else"              { (\ p s -> TELSE p) }
  "end"               { (\ p s -> TEND p) }
  "new"               { (\ p s -> TNEW p) }
  "in"                { (\ p s -> TIN p) }
  "private"           { (\ p s -> TPRIVATE p) }
  "dishonest"         { (\ p s -> TDISHONEST p) }
  "Protocol"          { (\ p s -> TPROTOCOL p) }
  "Sigma0"            { (\ p s -> TSIGMA0 p) }
  "Sigma"             { (\ p s -> TSIGMA p) }
  "Cells"             { (\ p s -> TCELLS p) }
  "Agents"            { (\ p s -> TAGENTS p) }
  "Knowledge"         { (\ p s -> TKNOWLEDGE p) }
  "Actions"	          { (\ p s -> TACIONS p) }
  "Bound"             { (\ p s -> TBOUND p) }
  "true"              { (\ p s -> TTRUE p) }
  "false"             { (\ p s -> TFALSE p) }
  "and"               { (\ p s -> TAND p) }
  "not"               { (\ p s -> TNOT p) }
  "or"                { (\ p s -> TOR p) }
  $alphaL $identChar* { (\ p s -> TCONST p s) }
  $alphaU $identChar* { (\ p s -> TVAR p s) }
  $digit+             { (\ p s -> TINT p (read s)) }

{

  data Token
   = TCONST AlexPosn String
   | TVAR AlexPosn String
   | TINT AlexPosn Int
   | TOPENP AlexPosn
   | TCLOSEP AlexPosn
   | TOPENB AlexPosn
   | TCLOSEB AlexPosn
   | TCOLON AlexPosn
   | TCOMMA AlexPosn
   | TOPENSQB AlexPosn
   | TCLOSESQB AlexPosn
   | TDIAMOND AlexPosn
   | TSTAR AlexPosn
   | TASSIGN AlexPosn
   | TEQUAL AlexPosn
   | TSLASH AlexPosn
   | TCHANNEL AlexPosn
   | TIF AlexPosn
   | TTHEN AlexPosn
   | TELSE AlexPosn
   | TEND AlexPosn
   | TNEW AlexPosn
   | TIN AlexPosn
   | TPRIVATE AlexPosn
   | TDISHONEST AlexPosn
   | TPROTOCOL AlexPosn
   | TSIGMA0 AlexPosn
   | TSIGMA AlexPosn
   | TCELLS AlexPosn
   | TAGENTS AlexPosn
   | TKNOWLEDGE AlexPosn
   | TACIONS AlexPosn
   | TBOUND AlexPosn
   | TTRUE AlexPosn
   | TFALSE AlexPosn
   | TAND AlexPosn
   | TNOT AlexPosn
   | TOR AlexPosn
   deriving (Eq,Show)

tokenPosn (TCONST p _) = p
tokenPosn (TVAR p _) = p
tokenPosn (TINT p _) = p
tokenPosn (TOPENP p) = p
tokenPosn (TCLOSEP p) = p
tokenPosn (TOPENB p) = p
tokenPosn (TCLOSEB p) = p
tokenPosn (TCOLON p) = p
tokenPosn (TCOMMA p) = p
tokenPosn (TOPENSQB p) = p
tokenPosn (TCLOSESQB p) = p
tokenPosn (TDIAMOND p) = p
tokenPosn (TSTAR p) = p
tokenPosn (TASSIGN p) = p
tokenPosn (TEQUAL p) = p
tokenPosn (TSLASH p) = p
tokenPosn (TCHANNEL p) = p
tokenPosn (TIF p) = p
tokenPosn (TTHEN p) = p
tokenPosn (TELSE p) = p
tokenPosn (TEND p) = p
tokenPosn (TNEW p) = p
tokenPosn (TIN p) = p
tokenPosn (TPRIVATE p) = p
tokenPosn (TDISHONEST p) = p
tokenPosn (TPROTOCOL p) = p
tokenPosn (TSIGMA0 p) = p
tokenPosn (TSIGMA p) = p
tokenPosn (TCELLS p) = p
tokenPosn (TAGENTS p) = p
tokenPosn (TKNOWLEDGE p) = p
tokenPosn (TACIONS p) = p
tokenPosn (TBOUND p) = p
tokenPosn (TTRUE p) = p
tokenPosn (TFALSE p) = p
tokenPosn (TAND p) = p
tokenPosn (TNOT p) = p
tokenPosn (TOR p) = p
tokenPosn x = error ("Not implemented tokenPosn for " ++ (show x))

}