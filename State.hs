module State where

import qualified Control.Monad.Trans.State as ST
import qualified Data.Map as M
import Data.Foldable (for_)
import Data.Char (isUpper)
import Types (Label, Msg, Agent, Marking(ToDo, Done), Frame, AgentFrames)

data State = State
  { counter    :: Int
  , frames     :: AgentFrames
  , pubFunc    :: [(String, Int)]
  , sigma0     :: [(String, Int)]
  , sigma      :: [(String, Int)]
  , sigmaPriv  :: [(String, Int)]
  , cellLabels :: [Label]
  , agents     :: [Agent]
  , hActors    :: [Label]
  , dActors    :: [Label]
  }
  deriving (Show)

initialState :: State
initialState = State 
  { counter    = 0
  , frames     = M.empty
  , pubFunc    = [("pair", 2), ("crypt", 3), ("scrypt", 3), ("sign", 2)]
  , sigma0     = []
  , sigma      = []
  , sigmaPriv  = []
  , cellLabels = []
  , agents     = []
  , hActors    = []
  , dActors    = []
  }

-- state monad
type MState = ST.StateT State []

get :: MState State
-- fetch the current state
get = ST.get

put :: State -> MState ()
-- update the state with new version
put = ST.put

runMState :: MState a -> State -> [(a, State)]
-- apply the state transformer on an state and return the final state and value
runMState = ST.runStateT

evalMState :: MState a -> State -> [a]
-- apply the state transformer on an state and return the final value
evalMState = ST.evalStateT

execMState :: MState a -> State -> [State]
-- apply the state transformer on an state and return the final state
execMState = ST.execStateT

isPublicId :: String -> Int -> MState Bool
-- check whether the identifier is public with correct arity
isPublicId id arity = do
  state <- get
  let isPubFunc = any (\(i,a) -> i == id && a == arity) (pubFunc state)
  inSig0 <- isInSigma0 id arity
  inSig <- isInSigma id arity
  return (isPubFunc || inSig0 || inSig)

isInSigma0 :: String -> Int -> MState Bool
isInSigma0 id arity = do
  state <- get
  return (any (\ (i,a) -> i == id && a == arity) (sigma0 state))

isInSigma :: String -> Int -> MState Bool
isInSigma id arity = do
  state <- get
  return (any (\ (i,a) -> i == id && a == arity) (sigma state))

isInSigmaPriv :: String -> Int -> MState Bool
isInSigmaPriv id arity = do
  state <- get
  return (any (\ (i,a) -> i == id && a == arity) (sigmaPriv state))

addToSigma0 :: String -> Int -> MState ()
addToSigma0 id arity = do
  state <- get
  put state { sigma0 = ((id, arity):(sigma0 state)) }

-- addToSigma :: String -> Int -> MState ()
-- addToSigma id arity = do
--   state <- get
--   put state { sigma = ((id, arity):(sigma state)) }

-- addToSigmaPriv :: String -> Int -> MState ()
-- addToSigmaPriv id arity = do
--   state <- get
--   put state { sigmaPriv = ((id, arity):(sigmaPriv state)) }

getPublicLabels :: MState [Label]
-- get all public labels from sigma0 and sigma
getPublicLabels = do
  state <- get
  let s0 = map fst (filter (\(_,a) -> a == 0) (sigma0 state))
  let s = map fst (filter (\(_,a) -> a == 0) (sigma state))
  return (s0 ++ s)

addInitialState :: Int -> [(String, Int)] -> [(String, Int)] -> [(String, Int)] -> [Agent] -> [String] -> [String] -> MState ()
addInitialState tCount sig0 sig sigP as hacts dacts = do
  state <- get
  let tLabels = map (\i -> ("T" ++ (show i), 0)) [1..tCount]
  let actors = map (\a -> (a,0)) (hacts ++ dacts)
  put state { 
    sigma0 = (("null", 0):tLabels ++ sig0 ++ actors ++ (sigma0 state)),
    sigma = (sig ++ (sigma state)),
    sigmaPriv = (sigP ++ (sigmaPriv state)),
    agents = as,
    hActors = hacts,
    dActors = dacts }

getVariableAgents :: MState [Agent]
getVariableAgents = do
  state <- get
  return (filter (isUpper.head) (agents state))

isAgent :: Agent -> MState Bool
isAgent agent = do
  state <- get
  return (elem agent (agents state))
