

module Types where

import Data.List (intercalate)
import qualified Data.Map as M

data Marking
  = ToDo
  | Done
  deriving (Show, Eq)

type Frame = M.Map Label (Msg, Marking)
type Agent = String
type AgentFrames = M.Map Agent Frame

type Var = String -- has to start with capital
type Const = String -- has to start with lowercase (represents function and constant names)
type Cell = Const

type Label = String

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
  deriving (Show)

data Msg
  = Atom String
  | Comp String [Msg]
  deriving (Show, Eq)

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

data Check
  = CTry Label Recipe
  | CIf (Formula Recipe)
  deriving (Show)

data Mode
  = MStar
  | MDiamond
  deriving (Show, Eq)

data NNProcess
  = NNSend Msg -- send(msg)
  | NNReceive Label -- receive(label)
  | NNTry Label Recipe [NNProcess] -- try check in processes catch nil
  | NNIf (Formula Recipe) [NNProcess] [NNProcess] -- if formula then processes else processes
  | NNNew [Label] -- new s1,s2
  | NNPickDomain Label [Label] -- * m in {m1, m2, m3, ...}
  | NNRead Label Cell Msg -- label := cell[msg]
  | NNWrite Cell Msg Msg -- cell[msg] := msg
  | NNRelease Mode (Formula Recipe) -- */<> formula
  | NNNil
  deriving (Show)

type NNTransaction = [NNProcess]
type NNCell = (Cell, Msg, Msg)
type NNDef = (String, Int)
