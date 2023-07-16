module ParserModel where

import Types (Agent, Msg(Atom, Comp), Formula, Mode)

type Var = String -- has to start with capital
type Const = String -- has to start with lowercase (represents function and constant names)
type Cell = Const

-- type AnnB = 


data PProcess
  = PNew [Var]
  | PChoice Mode Var [Const]
  | PRead Var Cell Msg
  | PWrite Cell Msg Msg
  | PRelease Mode Formula
  | PSend Int Msg
  | PReceive Int Msg
  deriving (Show, Eq)

data Action
  = Local Agent PProcess
    -- actions as "A: new N", where "B" is Agent "new N" is PProcess
  | Comm Agent Agent Msg
    -- actions as "A -> B: N", where "A" and "B" are corresponding agents and "N" is the message
  | PIf Agent Formula [Action] [Action]
    -- actions as "B: if f then as1 else as2 end", where "B" is agent, "f" is the Formula
    -- "as1" and "as2" are corresponding then and else actions
  deriving (Show, Eq)