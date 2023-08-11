module State where

import qualified Control.Monad.Trans.State as ST
import qualified Data.Map as Map
import Data.Foldable (for_)
import Data.Char (isUpper)
import Types (Label, Msg, Agent, Marking(ToDo, Done), Frame, AgentFrames)

--TODO: whole analysis could be just per agent (no need for AgentFrames)

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
  , txnCounter :: Int
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
  , txnCounter = 0
  }

-- -- state monad
-- type MState = ST.StateT State []

-- get :: MState State
-- -- fetch the current state
-- get = ST.get

-- put :: State -> MState ()
-- -- update the state with new version
-- put = ST.put

-- runMState :: MState a -> State -> [(a, State)]
-- -- apply the state transformer on an state and return the final state and value
-- runMState = ST.runStateT

-- evalMState :: MState a -> State -> [a]
-- -- apply the state transformer on an state and return the final value
-- evalMState = ST.evalStateT

-- execMState :: MState a -> State -> [State]
-- -- apply the state transformer on an state and return the final state
-- execMState = ST.execStateT

isPublicId :: String -> Int -> State -> Bool
-- check whether the identifier is public with correct arity
isPublicId id arity s = do
  any (\(i,a) -> i == id && a == arity) (pubFunc s)
  -- TODO: add sigma0 and sigma public to pubFunc
  -- inSig0 <- isInSigma0 id arity
  -- inSig <- isInSigma id arity
  -- return (isPubFunc || inSig0 || inSig)

-- isInSigma0 :: String -> Int -> MState Bool
-- isInSigma0 id arity = do
--   state <- get
--   return (any (\ (i,a) -> i == id && a == arity) (sigma0 state))

-- isInSigma :: String -> Int -> MState Bool
-- isInSigma id arity = do
--   state <- get
--   return (any (\ (i,a) -> i == id && a == arity) (sigma state))

-- isInSigmaPriv :: String -> Int -> MState Bool
-- isInSigmaPriv id arity = do
--   state <- get
--   return (any (\ (i,a) -> i == id && a == arity) (sigmaPriv state))

-- addToSigma0 :: String -> Int -> MState ()
-- addToSigma0 id arity = do
--   state <- get
--   put state { sigma0 = ((id, arity):(sigma0 state)) }

-- addToSigma :: String -> Int -> MState ()
-- addToSigma id arity = do
--   state <- get
--   put state { sigma = ((id, arity):(sigma state)) }

-- addToSigmaPriv :: String -> Int -> MState ()
-- addToSigmaPriv id arity = do
--   state <- get
--   put state { sigmaPriv = ((id, arity):(sigmaPriv state)) }

-- getPublicLabels :: MState [Label]
-- -- get all public labels from sigma0 and sigma
-- getPublicLabels = do
--   state <- get
--   let s0 = map fst (filter (\(_,a) -> a == 0) (sigma0 state))
--   let s = map fst (filter (\(_,a) -> a == 0) (sigma state))
--   return (s0 ++ s)

freshLabel :: State -> (Label, State)
-- generate a fresh label
freshLabel s = do
  let i = counter s
  let label = "X" ++ show i
  (label, (s { counter = i + 1 }))

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

-- getVariableAgents :: MState [Agent]
-- getVariableAgents = do
--   state <- get
--   return (filter (isUpper.head) (agents state))

-- isAgent :: Agent -> MState Bool
-- isAgent agent = do
--   state <- get
--   return (elem agent (agents state))

-- getFrameForAgent :: Agent -> MState Frame
-- -- helper to get a frame for a given agent
-- getFrameForAgent agent = do
--   state <- get
--   let afs = (frames state)
--   return (Map.findWithDefault Map.empty agent afs)

-- putFrameForAgent :: Agent -> Frame -> MState ()
-- -- helper to update a frame for a given agent
-- putFrameForAgent agent frame = do
--   state <- get
--   let afs = (frame state)
--   put state { frame = Map.insert agent frame afs }

addToXVars :: Label -> State -> State
addToXVars var s =
  s { xVars = (var:(xVars s)) }

clearXVars :: State -> State
clearXVars s =
  s { xVars = [] }

setYVars :: [Label] -> State -> State
setYVars yVars s =
  s { yVars = yVars }