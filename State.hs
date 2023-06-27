module State where

import qualified Control.Monad.Trans.State as ST
import qualified Data.Map as M
import Data.Foldable (for_)
import Types (Label, Msg, Agent, Marking(ToDo, Done), Frame, AgentFrames)

data State = State
  { counter    :: Int
  , frames     :: AgentFrames
  , pubFunc    :: [(String, Int)]
  , sigma0     :: [(String, Int)]
  , sigma      :: [(String, Int)]
  , sigmaPriv  :: [(String, Int)]
  , cellLabels :: [Label]
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

isPublicFunction :: String -> Int -> MState Bool
-- check whether the identifier is a public function with correct arity
isPublicFunction func arity = do
  state <- get
  return (any (\ (f, a) -> f == func && a == arity) (pubFunc state))

addPublicFunction :: String -> Int -> MState ()
addPublicFunction func arity = do
  state <- get
  put state { pubFunc = ((func, arity):(pubFunc state)) }

isInSigma0 :: String  -> MState Bool
isInSigma0 id = do
  state <- get
  return (any (\ (i,_) -> i == id) (sigma0 state))

isInSigma :: String-> MState Bool
isInSigma id = do
  state <- get
  return (any (\ (i,_) -> i == id) (sigma0 state))

isInSigmaPriv :: String -> MState Bool
isInSigmaPriv id = do
  state <- get
  return (any (\ (i,_) -> i == id) (sigma0 state))

addToSigma0 :: String -> Int -> MState ()
addToSigma0 id arity = do
  state <- get
  put state { sigma0 = ((id, arity):(sigma0 state)) }

addToSigma :: String -> Int -> MState ()
addToSigma id arity = do
  state <- get
  put state { sigma = ((id, arity):(sigma state)) }

addToSigmaPriv :: String -> Int -> MState ()
addToSigmaPriv id arity = do
  state <- get
  put state { sigmaPriv = ((id, arity):(sigmaPriv state)) }

getPublicLabels :: MState [Label]
getPublicLabels = do
  state <- get
  let s0 = map fst (filter (\(_,a) -> a == 0) (sigma0 state))
  let s = map fst (filter (\(_,a) -> a == 0) (sigma state))
  return (s0 ++ s)

addTransactionLabelsToSigma0 :: Int -> MState()
addTransactionLabelsToSigma0 tCount = do
  let labels = map (\i -> "T" ++ (show i)) [1..tCount]
  for_ labels (\l -> addToSigma0 l 0)
