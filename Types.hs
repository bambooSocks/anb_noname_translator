

module Types where

import Data.List (intercalate)

-- type AST = ([Declaration], [(Message,Message)], [Action], Int)

-- data Term v f
--   = Var v
--   | Comp f [Term v f]
--   deriving (Eq, Ord, Show)

-- type Id = String
-- type Message = Term Variable Function

-- data Declaration
--   = Sigma0 [Sigma0Declaration]
--   | Sigma [SigmaDeclaration]

-- data Sigma0Declaration
--   = Agent Id
--   | AgentDomain Id [Id]
--   | Number Id

-- data SigmaDeclaration
--   = SNumber Id Bool
--   | Fun 

data Msg
  = Atom String
  | Comp String [Msg]
  deriving (Show, Eq)

data Recipe
  = RAtom Label
  | RComp String [Recipe]
  deriving (Show, Eq)

type Label = String
type Cell = String
type Agent = String
type CheckInfo = (Label, Recipe)
data Check
  = CTry CheckInfo
  | CIf CheckInfo
  deriving (Show)

data Formula
  = BNot Formula
  | BAnd Formula Formula
  | BEq Recipe Recipe -- why is this recipe?
  | BTrue
  deriving (Show)

-- data LocalAction -- convert to just action including the communication actions and agent info
--   = LNew [String]
--   | LPickDomain Msg [Msg]
--   | LRead Label Cell Msg
--   | LWrite Cell Msg Msg
--   | LRelease Mode Formula
--   deriving (Show)

data Action
  = ASend Agent Msg
  | AReceive Agent Label Msg Label
  | AIf Agent Formula [Action] [Action]
  | ANew [String]
  | APickDomain Msg [Msg]
  | ARead Label Cell Msg
  | AWrite Cell Msg Msg
  | ARelease Mode Formula
  deriving (Show)

data Mode
  = MStar
  | MDiamond
  deriving (Show)

data NNProcess
  = PSend Agent Msg -- send(msg)
  | PReceive Agent Label Msg -- receive(label)
  | PTry Agent CheckInfo [NNProcess] -- try check in processes catch nil
  | PCheckIf Agent CheckInfo [NNProcess] -- if check then processes else nil -- TODO: maybe remove later
  | PIf Agent Formula [NNProcess] -- if formula then processes else nil
  | PNew Agent [String] -- new s1,s2
  | PPickDomain Agent Msg [Msg] -- * m in {m1, m2, m3, ...}
  | PRead Agent Label Cell Msg -- label := cell[msg]
  | PWrite Agent Cell Msg Msg -- cell[msg] := msg
  | PRelease Agent Mode Formula -- */<> formula
  | PNil
  deriving (Show)

type NNTransaction = [NNProcess]
type NNCell = (Cell, Msg, Msg)

msgToStr :: Msg -> String
msgToStr (Atom x) = x
msgToStr (Comp id args) = do
  let argStr = intercalate "," (map msgToStr args)
  id ++ "(" ++ argStr ++ ")"

recipeToStr :: Recipe -> String
recipeToStr (RAtom x) = x
recipeToStr (RComp id args) = do
  let argStr = intercalate "," (map recipeToStr args)
  id ++ "(" ++ argStr ++ ")"

formulaToStr :: Formula -> String
formulaToStr (BAnd f1 f2) = do
  "(" ++ (formulaToStr f1) ++ " && " ++ (formulaToStr f2) ++ ")"
formulaToStr (BNot f) = do
  "(not " ++ (formulaToStr f) ++ ")"
formulaToStr (BEq r1 r2) = do
  "(" ++ (recipeToStr r1) ++ " = " ++ (recipeToStr r2) ++ ")"
ormulaToStr (BTrue) = do
  "true"

unableErrorMsg :: Msg -> String
unableErrorMsg msg = 
  "Unable to deduce a recipe for " ++ (msgToStr msg) ++ " from agent's frame"