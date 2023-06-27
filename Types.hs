

module Types where

import Data.List (intercalate)
import qualified Data.Map as M

data Action
  = Local Agent AgentAction
  | Comm Agent Agent Msg
  | If Agent Formula [Action] [Action]
  deriving (Show, Eq)

data Marking
  = ToDo
  | Done
  deriving (Show, Eq)

type Frame = M.Map Label (Msg, Marking)
type AgentFrames = M.Map Agent Frame

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
  deriving (Show, Eq)

data AgentAction
  = ASend Agent Msg
  | AReceive Agent Msg Label
  | AIf Agent Formula [AgentAction] [AgentAction]
  | ANew Agent [Label]
  | APickDomain Agent Label [Label]
  | ARead Agent Label Cell Msg
  | AWrite Agent Cell Msg Msg
  | ARelease Agent Mode Formula
  | ANil
  deriving (Show, Eq)

data Mode
  = MStar
  | MDiamond
  deriving (Show, Eq)

data NNProcess
  = PSend Msg -- send(msg)
  | PReceive Label -- receive(label)
  | PTry CheckInfo [NNProcess] -- try check in processes catch nil
  | PIf Formula [NNProcess] [NNProcess] -- if formula then processes else processes
  | PNew [Label] -- new s1,s2
  | PPickDomain Label [Label] -- * m in {m1, m2, m3, ...}
  | PRead Label Cell Msg -- label := cell[msg]
  | PWrite Cell Msg Msg -- cell[msg] := msg
  | PRelease Mode Formula -- */<> formula
  | PNil
  deriving (Show)

type NNTransaction = [NNProcess]
type NNCell = (Cell, Msg, Msg)
