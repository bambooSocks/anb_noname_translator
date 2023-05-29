module Verification where

import Data.Foldable (for_)
import qualified Control.Monad.Trans.State as ST
import qualified Data.Map as M
import Types (Msg(Atom, Comp), Agent)

data Recipe
  = RAtom Label
  | RComp Label [Recipe]
  deriving (Show, Eq)

type Label = String

data Marking -- maybe need TryAgain
  = ToDo
  | Done
  deriving (Show, Eq)

type Frame = M.Map Label (Msg, Marking)
type AgentFrames = M.Map Agent Frame

data State = State
  { counter           :: Int
  , frames            :: AgentFrames
  , synonyms          :: [(Recipe, Recipe)] -- TODO: figure out if needed?
  , pubFunc           :: [String]
  , customDestructors :: [(Msg, Msg)]
  }
  deriving (Show)

initialState :: State
initialState = State 
  { counter           = 0
  , frames            = M.empty
  , synonyms          = []
  , pubFunc           = ["pair", "crypt", "scrypt", "sign"]
  , customDestructors = []
  }

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

freshLabel :: MState Label
-- generate a fresh label
freshLabel = do
  state <- get
  let i = counter state
  let label = "x" ++ show i
  put state { counter = i + 1 }
  return label

register :: Agent -> Msg -> Marking -> Label -> MState ()
-- register a new label and corresponding message in the frame
register agent msg marking label = do
  frame <- getFrameForAgent agent
  let newFrame = M.insert label (msg, marking) frame
  putFrameForAgent agent newFrame
  decomposeToDo agent

registerFresh :: Agent -> Msg -> Marking -> MState ()
-- generate a new label and register message with given marking
registerFresh agent msg marking = do
  label <- freshLabel
  register agent msg marking label

getFrameForAgent :: Agent -> MState Frame
-- helper to get a frame for a given agent
getFrameForAgent agent = do
  state <- get
  let afs = (frames state)
  return (M.findWithDefault M.empty agent afs)

putFrameForAgent :: Agent -> Frame -> MState ()
-- helper to update a frame for a given agent
putFrameForAgent agent frame = do
  state <- get
  let afs = (frames state)
  put state { frames = M.insert agent frame afs }

isInAgentsFrame :: Agent -> Msg -> MState Bool
-- checks whether the message is in agent's frame
isInAgentsFrame agent msg = do
  frame <- getFrameForAgent agent
  let msgs = M.map fst frame
  return (any (msg ==) msgs)

initAgentsFrame :: Agent -> [Msg] -> MState ()
initAgentsFrame agent [] = do
  return ()
initAgentsFrame agent (msg:msgs) = do
  registerFresh agent msg ToDo -- not sure if we want it Done or ToDo (if it is Atoms only easier to set to Done)
  initAgentsFrame agent msgs

decomposeToDo :: Agent -> MState ()
-- try to decomposes all messages in frame that are marked ToDo
decomposeToDo agent = do
  frame <- getFrameForAgent agent
  let msgsToDecompose = M.filter (\(_, mark) -> mark == ToDo) frame
  for_ (M.toList msgsToDecompose) (\(label,(msg, _)) -> decompose agent msg label)
  newFrame <- getFrameForAgent agent
  if frame /= newFrame then
    decomposeToDo agent
  else
    return ()

decompose :: Agent -> Msg  -> Label -> MState ()
decompose agent msg@(Atom _) label = do
  register agent msg Done label
-- pair decomposition
decompose agent msg@(Comp "pair" (f:s:[])) label = do
  register agent msg Done label
  registerFresh agent f ToDo
  registerFresh agent s ToDo
-- scrypt decomposition, has to have a key sk in frame to receive the msg
decompose agent msg@(Comp "scrypt" (sk:dm:[])) label = do
  fr <- getFrameForAgent agent
  if any (\(m, _) -> sk == m) fr then do
    register agent msg Done label
    registerFresh agent dm ToDo
  else
    return ()
-- crypt decomposition, has to have a private key inv(pk) in frame to receive the msg
decompose agent msg@(Comp "crypt" (pk:dm:[])) label = do
  fr <- getFrameForAgent agent
  if any (\(m, _) -> (Comp "inv" [pk]) == m) fr then do
    register agent msg Done label
    registerFresh agent dm ToDo
  else
    return ()
-- sign decomposition has, to have a public key pk in frame to receive the msg
decompose agent msg@(Comp "sign" ((Comp "inv" (pk:[])):dm:[])) label = do
  fr <- getFrameForAgent agent
  if any (\(m, _) -> pk == m) fr then do
    register agent msg Done label
    registerFresh agent dm ToDo
  else
    return ()
-- default decomposition - TODO: should be later extended to support custom deconstructors
decompose agent msg label = do
  return ()

isPublicFunction :: String -> MState Bool
-- check whether the identifier is a public function
isPublicFunction func = do
  state <- get
  return (any (func ==) (pubFunc state))

canDeduceFromFrame :: Agent -> Msg -> MState Bool
-- checks whether an atom can be deduced from the frame
canDeduceFromFrame agent msg@(Atom _) = do
  isInAgentsFrame agent msg
-- checks whether a composition can be deduced from the frame
canDeduceFromFrame agent msg@(Comp id args) = do
  isInFrame <- isInAgentsFrame agent msg
  idAllowed <- isPublicFunction id
  allArgsCanBeDeduced <- canDeduceManyFromFrame agent args
  return (isInFrame || (idAllowed && allArgsCanBeDeduced))

canDeduceManyFromFrame :: Agent -> [Msg] -> MState Bool
-- checks whether a list of messages can be deduced from agent's frame
canDeduceManyFromFrame agent [] = do
  return True
canDeduceManyFromFrame agent (msg:msgs) = do
  canDeduce <- canDeduceFromFrame agent msg
  canRestDeduce <- canDeduceManyFromFrame agent msgs
  return (canDeduce && canRestDeduce)

receive :: Agent -> Msg -> MState ()
receive agent msg =
  registerFresh agent msg ToDo

main :: IO ()
main = do
  ---- pair example
  -- let x = Comp "pair" [Atom "a", Comp "pair" [Atom "b", Atom "c"]]
  ---- scrypt example
  -- let x = Comp "pair" [Atom "sk", (Comp "scrypt" [Atom "sk", Atom "secret"])]
  -- let x = Comp "pair" [(Comp "scrypt" [Atom "sk", Atom "secret"]), Atom "sk"]
  let x = Comp "scrypt" [Atom "sk", Atom "secret"]
  ---- crypt example
  -- let x = Comp "pair" [Comp "inv" [Atom "pk"], (Comp "crypt" [Atom "pk", Atom "secret"])]
  -- let x = Comp "crypt" [Atom "pk", Atom "secret"]
  ---- sign example
  -- let x = Comp "pair" [Atom "pk", (Comp "sign" [Comp "inv" [Atom "pk"], Atom "secret"])]
  -- let x = Comp "sign" [Comp "inv" [Atom "pk"], Atom "secret"]
  ---- test for deducing scrypt(sk, secret)
  -- let x = Comp "pair" [Atom "secret", Atom "sk"]
  let xx = Atom "sk"
  let initKnowledgeA = [Atom "A", Comp "pk" [Atom "A"], Comp "inv" [Comp "pk" [Atom "A"]]]
  let stt0 = initAgentsFrame "A" initKnowledgeA
  let stt1 = receive "A" x
  let st1:[] = execMState stt1 initialState
  let canDeduce = evalMState (canDeduceFromFrame "A" (Comp "scrypt" [Atom "sk", Atom "secret"])) st1
  let canDeduce2 = evalMState (canDeduceFromFrame "A" (Comp "crypt" [Atom "sk", Atom "secret"])) st1
  let stt2 = receive "A" xx
  let st2:[] = execMState stt2 st1
  let canDeduce3 = evalMState (canDeduceFromFrame "A" (Comp "crypt" [Atom "sk", Atom "secret"])) st2
  putStrLn $ show st1
  putStrLn $ show st2
  putStrLn $ show canDeduce
  putStrLn $ show canDeduce2
  putStrLn $ show canDeduce3