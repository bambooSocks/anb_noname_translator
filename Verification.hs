module Verification where

import Data.Foldable (for_, find)
import qualified Control.Monad.Trans.State as ST
import qualified Data.Map as M
import Types (Msg(Atom, Comp), 
          Agent, Label,
          Check(CTry, CIf), 
          Recipe(RAtom, RComp),
          unableErrorMsg)

data Marking
  = ToDo
  | Done
  deriving (Show, Eq)

type Frame = M.Map Label (Msg, Marking)
type AgentFrames = M.Map Agent Frame

data State = State
  { counter           :: Int
  , frames            :: AgentFrames
  , pubFunc           :: [String]
  , customDestructors :: [(Msg, Msg)]
  }
  deriving (Show)

initialState :: State
initialState = State 
  { counter           = 0
  , frames            = M.empty
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
  let label = "X" ++ show i
  put state { counter = i + 1 }
  return label

--TODO: merge register and registerFresh to check the frame if it is in frame then just 
--      update the msg and marking otherwise create new label

register :: Agent -> Msg -> Marking -> Label -> MState ()
-- register a new label and corresponding message in the frame
register agent msg marking label = do
  frame <- getFrameForAgent agent
  let newFrame = M.insert label (msg, marking) frame
  putFrameForAgent agent newFrame

registerFresh :: Agent -> Msg -> Marking -> MState Label
-- generate a new label and register message with given marking
registerFresh agent msg marking = do
  label <- freshLabel
  register agent msg marking label
  return label

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

getLabel :: Agent -> Msg -> String -> MState Label
getLabel agent msg errMsg = do
  frame <- getFrameForAgent agent
  let res = find (\ (_,(m,_)) -> m == msg) (M.toList frame)
  case res of
    Just (label,_) -> do return label
    Nothing -> error errMsg

getRecipe :: Agent -> Msg -> MState Recipe
getRecipe agent msg@(Atom x) = do
  label <- getLabel agent msg (unableErrorMsg msg)
  return (RAtom label)
getRecipe agent msg@(Comp id args) = do
  isInFrame <- isInAgentsFrame agent msg
  if isInFrame then do
    label <- getLabel agent msg "Failed to find message in the agent's frame"
    return (RAtom label)
  else do
    idAllowed <- isPublicFunction id
    allArgsCanBeDeduced <- canDeduceManyFromFrame agent args
    if (idAllowed && allArgsCanBeDeduced) then do
      convertedArgs <- getRecipes agent args
      return (RComp id convertedArgs)
    else error (unableErrorMsg msg) 

getRecipes :: Agent -> [Msg] -> MState [Recipe]
getRecipes agent [] = do
  return []
getRecipes agent (msg:msgs) = do
  r <- getRecipe agent msg
  rs <- getRecipes agent msgs
  return (r:rs)

isToDo :: Agent -> Label -> MState Bool
isToDo agent label = do
  frame <- getFrameForAgent agent
  let res = M.lookup label frame
  case res of
    Just (_,m) -> return (m == ToDo)
    Nothing -> error "Couldn't find a label"

decomposeToDo :: Agent -> MState [Check]
-- try to decomposes all messages in frame that are marked ToDo
decomposeToDo agent = do
  frame <- getFrameForAgent agent
  let msgsToDecompose = M.filter (\(_, mark) -> mark == ToDo) frame
  decomposeFrame agent (M.toList msgsToDecompose)

decomposeFrame :: Agent -> [(Label,(Msg,Marking))] -> MState [Check]
decomposeFrame agent [] = do
  return []
decomposeFrame agent ((label,(msg,_)):rest) = do
  ch <- decompose agent msg label
  chs <- decomposeFrame agent rest
  return (ch ++ chs)

decompose :: Agent -> Msg -> Label -> MState [Check]
decompose agent msg@(Atom _) label = do
  register agent msg Done label
  return []
-- pair decomposition
decompose agent msg@(Comp "pair" (f:s:[])) label = do
  register agent msg Done label
  label2 <- registerFresh agent f ToDo
  label3 <- registerFresh agent s ToDo
  chs <- decomposeToDo agent
  return ([CTry (label2, RComp "proj1" [RAtom label]), CTry (label3, RComp "proj2" [RAtom label])] ++ chs)
-- scrypt decomposition, has to have a key sk in frame to receive the msg
decompose agent msg@(Comp "scrypt" (sk:dm:[])) label = do
  cond <- canDeduceFromFrame agent sk
  if cond then do
    register agent msg Done label
    label2 <- registerFresh agent dm ToDo
    skRecipe <- getRecipe agent sk
    chs <- decomposeToDo agent
    return ([CTry (label2, RComp "dscrypt" [skRecipe, RAtom label])] ++ chs)
  else
    return []
-- crypt decomposition, has to have a private key inv(pk) in frame to receive the msg
decompose agent msg@(Comp "crypt" (pk:dm:[])) label = do
  cond <- canDeduceFromFrame agent (Comp "inv" [pk])
  if cond then do
    register agent msg Done label
    label2 <- registerFresh agent dm ToDo
    invPkRecipe <- getRecipe agent (Comp "inv" [pk])
    chs <- decomposeToDo agent
    return ([CTry (label2, RComp "dcrypt" [invPkRecipe, RAtom label])] ++ chs)
  else
    return []
-- sign decomposition, has to have a public key pk in frame to receive the msg
decompose agent msg@(Comp "sign" ((Comp "inv" (pk:[])):dm:[])) label = do
  cond <- canDeduceFromFrame agent pk
  if cond then do
    register agent msg Done label
    label2 <- registerFresh agent dm ToDo
    pkRecipe <- getRecipe agent pk
    chs <- decomposeToDo agent
    return ([CTry (label2, RComp "open" [pkRecipe, RAtom label])] ++ chs)
  else
    return []
-- inv decomposition
decompose agent msg@(Comp "inv" (pk:[])) label = do
  cond <- canDeduceFromFrame agent pk
  if cond then do
    register agent msg Done label
    label2 <- getLabel agent pk "Could not find label for public key"
    return ([CTry (label2, RComp "pubk" [RAtom label])])
  else do
    register agent msg Done label
    label2 <- registerFresh agent pk ToDo
    return ([CTry (label2, RComp "pubk" [RAtom label])])
-- default decomposition
decompose agent msg@(Comp id args) label = do
  cond <- canDeduceFromFrame agent msg
  --cond2 <- isToDo agent label --TODO: investigate why is it called twice
  if cond then do
    register agent msg Done label
    argsRecipes <- getRecipes agent args
    return ([CIf (label, RComp id argsRecipes)])
  else
    return []

isPublicFunction :: String -> MState Bool
-- check whether the identifier is a public function
isPublicFunction func = do
  state <- get
  return (any (func ==) (pubFunc state))

addPublicFunction :: String -> MState ()
addPublicFunction func = do
  state <- get
  put state { pubFunc = (func:(pubFunc state)) }

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

receive :: Agent -> Msg -> MState (Label,[Check])
receive agent msg = do
  label <- freshLabel
  register agent msg ToDo label
  checks <- decomposeToDo agent
  return (label, checks)

main :: IO ()
main = do
  ---- pair example
  -- let x = Comp "pair" [Atom "a", Comp "pair" [Atom "b", Atom "c"]]
  ---- scrypt example
  -- let x = Comp "pair" [Atom "sk", (Comp "scrypt" [Atom "sk", Atom "secret"])]
  -- let x = Comp "pair" [(Comp "scrypt" [Atom "sk", Atom "secret"]), Atom "sk"]
  -- let x = Comp "scrypt" [Atom "sk", Atom "secret"]
  ---- crypt example
  -- let x = Comp "pair" [Comp "inv" [Atom "pk"], (Comp "crypt" [Atom "pk", Atom "secret"])]
  -- let x = Comp "crypt" [Atom "pk", Atom "secret"]
  ---- sign example
  -- let x = Comp "pair" [Atom "pk", (Comp "sign" [Comp "inv" [Atom "pk"], Atom "secret"])]
  -- let x = Comp "sign" [Comp "inv" [Atom "pk"], Atom "secret"]
  ---- test for deducing scrypt(sk, secret)
  -- let x = Comp "pair" [Atom "secret", Atom "sk"]
  
  -- composed key example
  -- pair(scrypt(scrypt(sk,N),secret),N)
  -- let x = Comp "pair" [Comp "scrypt" [Comp "scrypt" [Atom "sk", Atom "N"] , Atom "secret"], Atom "N"]
  -- let xx = Atom "sk"

  -- composition check example
  let x = Comp "pair" [Atom "sk", Atom "N"]
  let xx = Comp "h" [Atom "sk", Atom "N"]




  let initKnowledgeA = [Atom "A", Comp "pk" [Atom "A"], Comp "inv" [Comp "pk" [Atom "A"]]]
  let stt0 = initAgentsFrame "A" initKnowledgeA
  let stt1 = receive "A" x
  let st1:[] = execMState stt1 initialState
  let canDeduce = evalMState (canDeduceFromFrame "A" (Comp "scrypt" [Atom "sk", Atom "secret"])) st1
  let canDeduce2 = evalMState (canDeduceFromFrame "A" (Atom "secret")) st1
  let stt2 = receive "A" xx
  let st2:[] = execMState stt2 st1
  let canDeduce3 = evalMState (canDeduceFromFrame "A" (Atom "secret")) st2
  putStrLn $ show st1
  putStrLn $ show st2
  putStrLn $ show canDeduce
  putStrLn $ show canDeduce2
  putStrLn $ show canDeduce3