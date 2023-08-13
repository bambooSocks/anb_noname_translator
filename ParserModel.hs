module ParserModel where

import Types (Agent, Msg(Atom, Comp), Formula, Mode, Var, Cell, Const)

type AnnB = (String, [Def], [SigmaDef], [AgentDef], [Knowledge], [CellDef], Action, Int)

type Def = (String,Int)

data SigmaDef
  = Public [Def]
  | Private [Def]
  deriving (Show, Eq)

data AgentDef
  = Honest [Agent]
  | Dishonest [Agent]
  deriving (Show, Eq)

type Knowledge = (String, [Msg])

-- in verbatim

type CellDef = (Cell, Msg, Msg)

data Action
  = Local Agent PProcess Action
  | Comm Agent Agent Msg Action
  | End
  deriving (Show, Eq)

data PProcess
  = PNew [Var]
  | PRead Var Cell Msg
  | PWrite Cell Msg Msg
  | PChoice Mode Var [Const]
  | PRelease Mode (Formula Msg)
  | PIf (Formula Msg) Action Action
  deriving (Show, Eq)