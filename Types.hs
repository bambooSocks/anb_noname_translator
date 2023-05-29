

module Types where

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

type Agent = String

data Cond = NotDefined
  deriving (Show)

data UAction
  = UNew [String]
  | UPickDomain String [String]
  deriving (Show)

data NNProcess
  = PSend Agent Msg
  | PReceive Agent Msg
  | PTry Agent Cond [NNProcess]
  | PNew Agent [String]
  | PPickDomain Agent String [String] -- TODO: revise this
  deriving (Show)

type NNTransaction = [NNProcess]