

module Types where

import Data.List (intercalate)
import qualified Data.Map as M

type Var = String -- has to start with capital
type Const = String -- has to start with lowercase (represents function and constant names)
type Agent = String
type Cell = Const

data Msg
  = Atom String
  | Comp String [Msg]
  deriving (Show, Eq)

type Label = String
data Recipe
  = RPub String
  | RLabel Label
  | RComp String [Recipe]
  deriving (Show, Eq)

data Formula t
  = BAnd (Formula t) (Formula t)
  | BOr (Formula t) (Formula t)
  | BNot (Formula t)
  | BTrue
  | BEq t t
  deriving(Show, Eq)

data Mode
  = MStar
  | MDiamond
  deriving (Show, Eq)

-- Parser model
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

-- TODO: add in verbatim transactions

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

-- Projection model

data Projection
  = Receive Msg Projection
  | Read Var Cell Msg Projection
  | Choice Mode Var [Const] Projection
  | If (Formula Msg) Projection Projection
  | New [Var] Projection
  | Send Msg Projection
  | Write Cell Msg Msg Projection
  | Release Mode (Formula Msg) Projection
  | TxnEnd Projection
  | Split Projection Projection
  | Nil
  deriving (Show, Eq)

-- Analysis model

type Frame = M.Map Label (Msg, Marking)
data Marking
  = ToDo
  | Done
  deriving (Show, Eq)

data Check
  = CTry Label Recipe
  | CIf (Formula Recipe)
  deriving (Show)

data Process
  = NReceive Label Process -- receive(label)
  | NTry Label Recipe Process Process -- try check in process catch process
  | NIf (Formula Recipe) Process Process -- if formula then processes else processes
  | NChoice Mode Label [Label] Process -- */<> m in {m1, m2, m3, ...}
  | NRead Label Cell Recipe Process -- label := cell[msg]
  | NNew [Label] Process -- new s1,s2
  | NSend Recipe Process -- send(msg)
  | NWrite Cell Recipe Recipe Process -- cell[msg] := msg
  | NRelease Mode (Formula Recipe) Process -- */<> formula
  | NNil
  | NBreak Process Process
  deriving (Show)
