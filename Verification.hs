module Verification where

import Types (Msg(Atom, Comp), 
          Agent, Label, NNCell,
          Check(CTry, CIf), 
          Recipe(RAtom, RComp),
          Marking(ToDo, Done), Frame)
import Data.Foldable (find)
import qualified Data.Map as M
import qualified Helper as H
import qualified State as S

freshLabel :: S.MState Label
-- generate a fresh label
freshLabel = do
  state <- S.get
  let i = S.counter state
  let label = "X" ++ show i
  S.put state { S.counter = i + 1 }
  return label

--TODO: merge register and registerFresh to check the frame if it is in frame then just 
--      update the msg and marking otherwise create new label

register :: Agent -> Msg -> Marking -> Label -> S.MState ()
-- register a new label and corresponding message in the frame
register agent msg marking label = do
  frame <- getFrameForAgent agent
  let newFrame = M.insert label (msg, marking) frame
  putFrameForAgent agent newFrame

registerFresh :: Agent -> Msg -> Marking -> S.MState Label
-- generate a new label and register message with given marking
registerFresh agent msg marking = do
  label <- freshLabel
  register agent msg marking label
  return label

getFrameForAgent :: Agent -> S.MState Frame
-- helper to get a frame for a given agent
getFrameForAgent agent = do
  state <- S.get
  let afs = (S.frames state)
  return (M.findWithDefault M.empty agent afs)

putFrameForAgent :: Agent -> Frame -> S.MState ()
-- helper to update a frame for a given agent
putFrameForAgent agent frame = do
  state <- S.get
  let afs = (S.frames state)
  S.put state { S.frames = M.insert agent frame afs }

isInAgentsFrame :: Agent -> Msg -> S.MState Bool
-- checks whether the message is in agent's frame
isInAgentsFrame agent msg = do
  frame <- getFrameForAgent agent
  let msgs = M.map fst frame
  return (any (msg ==) msgs)

initAgentsFrame :: Agent -> [Msg] -> S.MState ()
initAgentsFrame agent [] = do
  return ()
initAgentsFrame agent (msg:msgs) = do
  registerFresh agent msg ToDo -- not sure if we want it Done or ToDo (if it is Atoms only easier to set to Done)
  initAgentsFrame agent msgs

tryGetLabel :: Agent -> Msg -> S.MState (Maybe Label)
tryGetLabel agent msg = do
  frame <- getFrameForAgent agent
  let res = find (\ (_,(m,_)) -> m == msg) (M.toList frame)
  case res of
    Just (label,_) -> return (Just label)
    Nothing -> return Nothing

tryGetRecipe :: Agent -> Msg -> S.MState (Maybe Recipe)
tryGetRecipe agent msg@(Atom x) = do
  label <- tryGetLabel agent msg
  case label of
    Just l -> return (Just (RAtom l))
    Nothing -> return Nothing
tryGetRecipe agent msg@(Comp id args) = do
  isInFrame <- isInAgentsFrame agent msg
  if isInFrame then do
    label <- tryGetLabel agent msg
    case label of
      Just l -> return (Just (RAtom l))
      Nothing -> return Nothing
  else do
    idAllowed <- S.isPublicFunction id (length args)
    convertedArgs <- tryGetRecipes agent args
    if (idAllowed && (all (Nothing /=) convertedArgs)) then do
      let recipes = H.extractRecipes convertedArgs
      return (Just (RComp id recipes))
    else return Nothing

tryGetRecipes :: Agent -> [Msg] -> S.MState [(Maybe Recipe)]
tryGetRecipes agent [] = do
  return []
tryGetRecipes agent (msg:msgs) = do
  r <- tryGetRecipe agent msg
  rs <- tryGetRecipes agent msgs
  return (r:rs)

decomposeToDo :: Agent -> S.MState [Check]
-- try to decomposes all messages in frame that are marked ToDo
decomposeToDo agent = do
  frame <- getFrameForAgent agent
  let msgsToDecompose = M.filter (\(_, mark) -> mark == ToDo) frame
  decomposeFrame agent (M.toList msgsToDecompose)

decomposeFrame :: Agent -> [(Label,(Msg,Marking))] -> S.MState [Check]
decomposeFrame agent [] = do
  return []
decomposeFrame agent ((label,(msg,_)):rest) = do
  ch <- decompose agent msg label
  chs <- decomposeFrame agent rest
  return (ch ++ chs)

decompose :: Agent -> Msg -> Label -> S.MState [Check]
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
    maybeSkRecipe <- tryGetRecipe agent sk
    let skRecipe = H.extractRecipe maybeSkRecipe
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
    maybeInvPkRecipe <- tryGetRecipe agent (Comp "inv" [pk])
    let invPkRecipe = H.extractRecipe maybeInvPkRecipe
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
    maybePkRecipe <- tryGetRecipe agent pk
    let pkRecipe = H.extractRecipe maybePkRecipe
    chs <- decomposeToDo agent
    return ([CTry (label2, RComp "open" [pkRecipe, RAtom label])] ++ chs)
  else
    return []
-- inv decomposition
decompose agent msg@(Comp "inv" (pk:[])) label = do
  cond <- canDeduceFromFrame agent pk
  if cond then do
    register agent msg Done label
    maybeLabel2 <- tryGetLabel agent pk
    let label2 = H.extractLabel maybeLabel2
    return ([CTry (label2, RComp "pubk" [RAtom label])])
  else do
    register agent msg Done label
    label2 <- registerFresh agent pk ToDo
    return ([CTry (label2, RComp "pubk" [RAtom label])])
-- default decomposition
decompose agent msg@(Comp id args) label = do
  cond <- canDeduceFromFrame agent msg
  argsRecipes <- tryGetRecipes agent args
  if cond && (all (Nothing /=) argsRecipes) then do
    register agent msg Done label
    let recipes = H.extractRecipes argsRecipes
    return ([CIf (label, RComp id recipes)])
  else
    return []

canDeduceFromFrame :: Agent -> Msg -> S.MState Bool
-- checks whether an atom can be deduced from the frame
canDeduceFromFrame agent msg@(Atom a) = do
  inFrame <- isInAgentsFrame agent msg
  inSigma0 <- S.isInSigma0 a
  inSigma <- S.isInSigma a
  return (inFrame || inSigma0)
-- checks whether a composition can be deduced from the frame
canDeduceFromFrame agent msg@(Comp id args) = do
  isInFrame <- isInAgentsFrame agent msg
  idAllowed <- S.isPublicFunction id (length args)
  allArgsCanBeDeduced <- canDeduceManyFromFrame agent args
  return (isInFrame || (idAllowed && allArgsCanBeDeduced))

canDeduceManyFromFrame :: Agent -> [Msg] -> S.MState Bool
-- checks whether a list of messages can be deduced from agent's frame
canDeduceManyFromFrame agent [] = do
  return True
canDeduceManyFromFrame agent (msg:msgs) = do
  canDeduce <- canDeduceFromFrame agent msg
  canRestDeduce <- canDeduceManyFromFrame agent msgs
  return (canDeduce && canRestDeduce)

receive :: Agent -> Msg -> S.MState (Label,[Check])
receive agent msg = do
  label <- freshLabel
  register agent msg ToDo label
  checks <- decomposeToDo agent
  return (label, checks)

-- TODO: clean up main

main :: IO ()
main = do
  -- composition check example
  let x = Comp "pair" [Atom "sk", Atom "N"]
  let xx = Comp "h" [Atom "sk", Atom "N"]

  let initKnowledgeA = [Atom "A", Comp "pk" [Atom "A"], Comp "inv" [Comp "pk" [Atom "A"]]]
  let stt0 = initAgentsFrame "A" initKnowledgeA
  let stt1 = receive "A" x
  let st1:[] = S.execMState stt1 S.initialState
  let canDeduce = S.evalMState (canDeduceFromFrame "A" (Comp "scrypt" [Atom "sk", Atom "secret"])) st1
  let canDeduce2 = S.evalMState (canDeduceFromFrame "A" (Atom "secret")) st1
  let stt2 = receive "A" xx
  let st2:[] = S.execMState stt2 st1
  let canDeduce3 = S.evalMState (canDeduceFromFrame "A" (Atom "secret")) st2
  putStrLn $ show st1
  putStrLn $ show st2
  putStrLn $ show canDeduce
  putStrLn $ show canDeduce2
  putStrLn $ show canDeduce3