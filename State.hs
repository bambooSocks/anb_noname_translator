module State where

import qualified Control.Monad.Trans.State as ST
import qualified Data.Map as Map
import Data.Foldable (for_)
import Data.Char (isUpper)
import Types (
  Label, Msg, Agent, Marking(ToDo, Done), Frame, Def, SigmaDef (Public, Private), AgentDef (Honest, Dishonest),
  Knowledge, RoleDef)

data Counter = Counter
  { varCounter :: Int
  , txnCounter :: Int
  , cells      :: [String]
  , pubLabels  :: [String]
  }
  deriving (Show)

initialCounter :: Counter
initialCounter = Counter 
  { varCounter = 0
  , txnCounter = 0
  , cells      = []
  , pubLabels  = []
  }

-- state monad
type MCounter = ST.StateT Counter []

get :: MCounter Counter
-- fetch the current state
get = ST.get

put :: Counter -> MCounter ()
-- update the state with new version
put = ST.put

runMCounter :: MCounter a -> Counter -> [(a, Counter)]
-- apply the state transformer on an state and return the final state and value
runMCounter = ST.runStateT

evalMCounter :: MCounter a -> Counter -> [a]
-- apply the state transformer on an state and return the final value
evalMCounter = ST.evalStateT

execMCounter :: MCounter a -> Counter -> [Counter]
-- apply the state transformer on an state and return the final state
execMCounter = ST.execStateT

freshVarLabel :: MCounter String
freshVarLabel = do
  c <- get
  let new = (varCounter c) + 1
  put c { varCounter = new }
  return ("X" ++ (show new))

freshTxnNo :: MCounter Int
freshTxnNo = do
  c <- get
  let new = (txnCounter c) + 1
  put c { txnCounter = new }
  return new

addCell :: String -> MCounter ()
addCell cell = do
  c <- get
  put c { cells = cell:(cells c) }

addPubLabel :: String -> MCounter ()
addPubLabel label = do
  c <- get
  put c { pubLabels = label:(pubLabels c) }

data Header = Header
  { s0    :: [Def]
  , sPub  :: [Def]
  , sPriv :: [Def]
  , hAgs  :: [Agent]
  , dAgs  :: [Agent]
  , roles :: Map.Map Agent [Agent]
  , know  :: Map.Map String [Msg]
  }
  deriving(Show)

getHeader :: [Def] -> [SigmaDef] -> [AgentDef] -> [RoleDef] -> [Knowledge] -> Header
getHeader sig0 sig ags roles know = do
  let (sPub, sPriv) = splitSigmaDef sig ([],[])
  let (hAgs, dAgs) = splitAgentDef ags ([],[])
  Header { s0    = sig0
         , sPub  = sPub
         , sPriv = sPriv
         , hAgs  = hAgs
         , dAgs  = dAgs
         , roles = Map.fromList roles
         , know  = Map.fromList know
         }

addToSigma0 :: [String] -> Header -> Header
addToSigma0 ls h = do
  let defs = map (\l -> (l, 0)) ls
  h { s0 = (s0 h) ++ defs }

data State = State
  { frame      :: Frame
  , pubFunc    :: [(String, Int)]
  , cellLabels :: [Label]
  , xVars      :: [String]
  , yVars      :: [String]
  }
  deriving (Show)

initialState :: State
initialState = State 
  { frame      = Map.empty
  , pubFunc    = [("pair", 2), ("crypt", 3), ("scrypt", 3), ("sign", 2), ("inv", 1)] -- TODO: this is temp
  , cellLabels = []
  , xVars      = []
  , yVars      = []
  }

splitSigmaDef :: [SigmaDef] -> ([Def], [Def]) -> ([Def], [Def])
splitSigmaDef [] (sPub, sPriv) =
  (sPub, sPriv)
splitSigmaDef ((Public d):sds) (sPub, sPriv) =
  splitSigmaDef sds (sPub ++ d, sPriv)
splitSigmaDef ((Private d):sds) (sPub, sPriv) =
  splitSigmaDef sds (sPub, sPriv ++ d)

splitAgentDef :: [AgentDef] -> ([Agent], [Agent]) -> ([Agent], [Agent])
splitAgentDef [] (hAgs, dAgs) =
  (hAgs, dAgs)
splitAgentDef ((Honest a):ads) (hAgs, dAgs) = 
  splitAgentDef ads (hAgs ++ a, dAgs)
splitAgentDef ((Dishonest a):ads) (hAgs, dAgs) = 
  splitAgentDef ads (hAgs, dAgs ++ a)

isPublicId :: String -> Int -> State -> Bool
-- check whether the identifier is public with correct arity
isPublicId id arity s = do
  any (\(i,a) -> i == id && a == arity) (pubFunc s)

register :: Msg -> Marking -> Label -> State -> State
-- register a new label and corresponding message in the frame
register msg marking label s = do
  let newFrame = Map.insert label (msg, marking) (frame s)
  s { frame = newFrame }

registerFresh :: Msg -> Marking -> State -> MCounter (Label, State)
-- generate a new label and register message with given marking
registerFresh msg marking s = do
  x <- freshVarLabel
  let s1 = register msg marking x s
  let s2 = addToXVars x s1
  return (x, s2)

registerManyFresh :: [Msg] -> Marking -> State -> MCounter ([Label], State)
-- generate a new label and register message with given marking
registerManyFresh [] _ s = do
  return ([], s)
registerManyFresh (msg:msgs) marking s = do
  (label, s1) <- registerFresh msg marking s
  (rest, s2) <- registerManyFresh msgs marking s1
  return (label:rest, s2)

addInitialState :: Agent -> Header -> [Msg] -> State -> State
addInitialState ag h msgs s = do
  let ags = map (\a -> (a, 0)) ((hAgs h) ++ (dAgs h))
  s { pubFunc = ((s0 h) ++ (sPub h) ++ ags ++ (pubFunc s)) }

addToXVars :: Label -> State -> State
addToXVars var s =
  s { xVars = (var:(xVars s)) }

clearXVars :: State -> State
clearXVars s =
  s { xVars = [] }

setYVars :: [Label] -> State -> State
setYVars yVars s =
  s { yVars = yVars }