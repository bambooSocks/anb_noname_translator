module State where

import qualified Control.Monad.Trans.State as ST
import qualified Data.Map as Map
import Data.Foldable (for_)
import Data.Char (isUpper)
import Types (Label, Msg, Agent, Marking(ToDo, Done), Frame, AgentFrames)

data Header = Header
  { sigma0     :: [(String, Int)]
  , sigma      :: [(String, Int)]
  , sigmaPriv  :: [(String, Int)]
  , agents     :: [String]
  , agentsDish :: [String]
  }
  deriving(Show)

data State = State
  { counter    :: Int
  , frame      :: Frame
  , pubFunc    :: [(String, Int)]
  , cellLabels :: [Label]
  , xVars      :: [String]
  , yVars      :: [String]
  }
  deriving (Show)

initialState :: State
initialState = State 
  { counter    = 0
  , frame      = Map.empty
  , pubFunc    = [("pair", 2), ("crypt", 3), ("scrypt", 3), ("sign", 2)]
  , cellLabels = []
  , xVars      = []
  , yVars      = []
  }


isPublicId :: String -> Int -> State -> Bool
-- check whether the identifier is public with correct arity
isPublicId id arity s = do
  any (\(i,a) -> i == id && a == arity) (pubFunc s)

freshLabel :: Int -> Label
-- generate a fresh label
freshLabel i = "X" ++ show i

freshTxnName :: Agent -> State -> (String, State)
-- generate a fresh label
freshTxnName ag s = do
  let i = txnCounter s
  let label = ag ++ "_" ++ show i
  (label, (s { txnCounter = i + 1 }))

register :: Msg -> Marking -> Label -> State -> State
-- register a new label and corresponding message in the frame
register msg marking label s = do
  let newFrame = Map.insert label (msg, marking) (frame s)
  s { frame = newFrame }

registerFresh :: Msg -> Marking -> State -> (Label, State)
-- generate a new label and register message with given marking
registerFresh msg marking s = do
  let (label, s1) = freshLabel s
  let s2 = register msg marking label s1
  (label, s2)

registerManyFresh :: [Msg] -> Marking -> State -> ([Label], State)
-- generate a new label and register message with given marking
registerManyFresh [] _ s =
  ([], s)
registerManyFresh (msg:msgs) marking s = do
  let (label, s1) = freshLabel s
  let s2 = register msg marking label s1
  let (rest, s3) = registerManyFresh msgs marking s2
  (label:rest, s3)

addInitialState :: Header -> [Msg] -> State -> State
addInitialState h msgs s = do
  let (_, s1) = registerManyFresh msgs ToDo s
  let ags = map (\a -> (a, 0)) ((agents h) ++ (agentsDish h))
  s1 { pubFunc = ((sigma0 h) ++ (sigma h) ++ ags ++ (pubFunc s1)) }

addToXVars :: Label -> State -> State
addToXVars var s =
  s { xVars = (var:(xVars s)) }

clearXVars :: State -> State
clearXVars s =
  s { xVars = [] }

setYVars :: [Label] -> State -> State
setYVars yVars s =
  s { yVars = yVars }