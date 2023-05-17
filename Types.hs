

module Types where

type AST = ([Declaration], [(Message,Message)], [Action], Int)

data Term v f
  = Var v
  | Comp f [Term v f]
  deriving (Eq, Ord, Show)

type Id = String
type Message = Term Variable Function

data Declaration
  = Sigma0 [Sigma0Declaration]
  | Sigma [SigmaDeclaration]

data Sigma0Declaration
  = Agent Id
  | AgentDomain Id [Id]
  | Number Id

data SigmaDeclaration
  = SNumber Id Bool
  | Fun 

