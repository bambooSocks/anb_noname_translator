module Analysis where

import Types (Msg (Atom, Comp), Agent, Recipe (RPub, RLabel, RComp), Action (Local, Comm, End), PProcess(PNew, PChoice, PIf, PRead, PRelease, PWrite), 
              Projection (Receive, Send, New, Choice, Read, Write, Release, If, Split, TxnEnd, Nil), Formula (BEq, BAnd, BNot, BTrue), Check (CTry, CIf),
              Process (NReceive, NTry, NIf, NChoice, NRead, NNew, NSend, NWrite, NRelease, NNil, NBreak),Knowledge, Label, CellDef, Marking (Done, ToDo))
import qualified Helper as H
import qualified Data.List as List
import qualified Data.Map as Map
import qualified State as S

project :: Agent -> Action -> Projection
-- the projection function - generates projection for an agent from action chain
project ag End =
  Nil
project ag (Local ag' (PNew vars) rest) =
  if ag == ag' then do
    let subProj = project ag rest
    New vars subProj
  else
    project ag rest
project ag (Local ag' (PChoice m v cs) rest) =
  if ag == ag' then do
    let subProj = project ag rest
    Choice m v cs subProj
  else
    project ag rest
project ag (Local ag' (PRead v c m) rest) =
  if ag == ag' then do
    let subProj = project ag rest
    Read v c m subProj
  else
    project ag rest
project ag (Local ag' (PWrite c m1 m2) rest) =
  if ag == ag' then do
    let subProj = project ag rest
    Write c m1 m2 subProj
  else
    project ag rest
project ag (Local ag' (PRelease m f) rest) =
  if ag == ag' then do
    let subProj = project ag rest
    Release m f subProj
  else
    project ag rest
project ag (Local ag' (PIf f rest1 rest2) End) =
  if ag == ag' then do
    let subProj1 = project ag rest1
    let subProj2 = project ag rest2
    If f subProj1 subProj2
  else do
    let subProj1 = project ag rest1
    let subProj2 = project ag rest2
    if subProj1 == Nil && subProj2 == Nil then
      Nil
    else
      Split subProj1 subProj2
project ag (Comm ag1 ag2 m End) =
  if ag == ag1 then do
    Send m Nil
  else
    Nil
project ag (Comm ag1 ag2 m rest) =
  if ag == ag1 then do
    let subProj = project ag rest
    Send m (TxnEnd subProj)
  else if ag == ag2 then do
    let subProj = project ag rest
    Receive m subProj
  else
    project ag rest
project ag _ =
  error ("Failed when retrieving projections for agent " ++ ag)

actionsToProjs :: Action -> [(Agent, Projection)]
-- split actions into corresponding projections
actionsToProjs a = do
  let agents = H.getActors a
  map (\ag -> (ag, (project ag a))) agents

getCells :: Action -> [CellDef]
getCells a = do
  let acs = List.nub (H.getActors a)
  let stepCells = map (\ac -> ("__INT_STEP_" ++ ac, Atom "S", Atom (ac ++ "0"))) acs
  stepCells

composeFormula :: Formula Msg -> S.State -> (Formula Recipe)
-- convert message formula to recipe formula
composeFormula (BAnd f1 f2) s = do
  let cf1 = composeFormula f1 s
  let cf2 = composeFormula f2 s
  BAnd cf1 cf2
composeFormula (BNot f) s = BNot (composeFormula f s)
composeFormula BTrue s = BTrue
composeFormula f@(BEq m1 m2) s = do
  let r1 = compose m1 s
  let r2 = compose m2 s
  if r1 == [] || r2 == [] then
    error ("Could not find a recipe for element in formula " ++ (H.msgFormulaToStr f))
  else
    BEq (head r1) (head r2)

getLabelsInFrame :: Msg -> S.State -> [Recipe]
-- get all labels in frame
getLabelsInFrame msg s = do
  let matching = Map.filter (\(m,_) -> m == msg) (S.frame s)
  map (\l -> RLabel l) (Map.keys matching)

composeMany :: [Msg] -> S.State -> [[Recipe]]
-- get recipes for several messages
composeMany [] _ = []
composeMany (msg:msgs) s = do
  let r = compose msg s
  let rs = composeMany msgs s
  (r:rs)

compose :: Msg -> S.State -> [Recipe]
-- compose plain with a validation check
compose msg s = do
  let rs = composePlain msg s
  if rs == [] then
    error ("Could not compose message: " ++ (H.msgToStr msg) ++ " from agent's frame")
  else
    rs

composePlain :: Msg -> S.State -> [Recipe]
-- get recipes for a message
composePlain msg@(Atom x) s = do
  let labelsInFrame = getLabelsInFrame msg s
  let isPub = S.isPublicId x 0 s
  if isPub then
    (RPub x):labelsInFrame
  else
    labelsInFrame
composePlain msg@(Comp x args) s = do
  let labelsInFrame = getLabelsInFrame msg s
  let isPub = S.isPublicId x (length args) s
  let rs = composeMany args s
  let argrs = map head rs
  if isPub then
    (RComp x argrs):labelsInFrame
  else do
    labelsInFrame

getAllRecipePairs :: Label -> [Recipe] -> [(Recipe,Recipe)]
-- find all pairs of recipes
getAllRecipePairs l rs = do
  let label = RLabel l
  map (\r -> (label, r)) (filter (label /=) rs)

setAllLabelsDone :: [Recipe] -> Msg -> S.State -> S.State
setAllLabelsDone [] _ s = s
setAllLabelsDone ((RLabel l):rs) msg s = do
  let s1 = setAllLabelsDone rs msg s
  S.register msg Done l s1
setAllLabelsDone (_:rs) msg s =
  setAllLabelsDone rs msg s

analyzeToDo :: S.State -> ([Check], S.State)
-- try to decomposes all messages in frame that are marked ToDo
analyzeToDo s = do
  case (List.find (\(_,(_,mark)) -> mark == ToDo) (Map.toList (S.frame s))) of
    Just (l,(msg,_)) -> do
      let (ch, s1) = analyze l msg s
      let (chs, s2) = analyzeToDo s1
      ((ch ++ chs), s2)
    Nothing -> ([], s)
  
analyze :: Label -> Msg -> S.State -> ([Check], S.State)
-- pair decomposition
analyze l msg@(Comp "pair" (fr:sc:[])) s = do
  let s1 = S.register msg Done l s
  let (l1, s2) = S.registerFresh fr ToDo s1
  let (l2, s3) = S.registerFresh sc ToDo s2
  let (chs, s4) = analyzeToDo s3
  (([CTry l1 (RComp "proj1" [RLabel l]), CTry l2 (RComp "proj2" [RLabel l])] ++ chs), s4)
-- scrypt decomposition, has to have a key sk in frame to receive the msg
analyze l msg@(Comp "scrypt" (sk:dm:r:[])) s = do
  let rs = compose sk s
  if rs /= [] then do
    let s1 = S.register msg Done l s
    let (l1, s2) = S.registerFresh dm ToDo s1
    let (chs, s3) = analyzeToDo s2
    (([CTry l1 (RComp "dscrypt" [(head rs), RLabel l])] ++ chs), s3)
  else
    ([], s)
-- crypt decomposition, has to have a private key inv(pk) in frame to receive the msg
analyze l msg@(Comp "crypt" (pk:dm:r:[])) s = do
  let rs = compose (Comp "inv" [pk]) s
  if rs /= [] then do
    let s1 = S.register msg Done l s
    let (l1, s2) = S.registerFresh dm ToDo s1
    let (chs, s3) = analyzeToDo s2
    (([CTry l1 (RComp "dcrypt" [(head rs), RLabel l])] ++ chs), s3)
  else
    ([], s)
-- sign decomposition, has to have a public key pk in frame to receive the msg
analyze l msg@(Comp "sign" ((Comp "inv" (pk:[])):dm:[])) s = do
  let rs = compose pk s
  if rs /= [] then do
    let s1 = S.register msg Done l s
    let (l1, s2) = S.registerFresh dm ToDo s1
    let (chs, s3) = analyzeToDo s2
    (([CTry l1 (RComp "open" [(head rs), RLabel l])] ++ chs), s3)
  else
    ([], s)
-- inv decomposition
analyze l msg@(Comp "inv" (pk:[])) s = do
  let rs = compose pk s
  if rs /= [] then do
    let s1 = S.register msg Done l s
    (([CIf (BEq (head rs) (RComp "pubk" [RLabel l]))]), s1)
  else
    ([], s)
-- default decomposition
analyze l msg s = do
  let rs = compose msg s
  if rs /= [] then do
    let ifChecks = map (\(a,b) -> CIf (BEq a b)) (getAllRecipePairs l rs)
    let s1 = setAllLabelsDone rs msg s
    (ifChecks, s1)
  else
    ([], s)
  
convertCheck :: Check -> Process -> Process
-- convert checks into processes
convertCheck (CTry l r) acc =
  NTry l r acc NNil
convertCheck (CIf cond) acc = do
  NIf cond acc NNil

endTxn :: Agent -> [Label] -> Int -> Process -> Process
-- create the ending processes of a transaction
endTxn ag [] tn p = do
  NWrite ("__INT_STEP_" ++ ag) (RPub "S") (RPub (ag ++ (show tn))) (NSend (RPub "S") p)
endTxn ag (x:xs) tn p = do
  NWrite ("__INT_MEM_" ++ x) (RPub "S") (RLabel x) (endTxn ag xs tn p)

startTxn :: Agent -> [Label] -> Int -> Process -> Process
startTxn ag ys tn p = do
  NReceive "S" (
    NRead "__INT_STEP" ("__INT_STEP_" ++ ag) (RPub "S") (
      NIf (BEq (RLabel "__INT_STEP") (RPub (ag ++ (show tn)))) (
        readAll ys p) NNil))

readAll :: [Label] -> Process -> Process
readAll [] p = p
readAll (y:ys) p = NRead y ("__INT_MEM_" ++ y) (RPub "S") p

getAgentKnowledge :: Agent -> [Knowledge] -> [Msg]
getAgentKnowledge ag kns =
  case (List.find (\(a,_) -> a == ag) kns) of
    Just (_, msgs) -> msgs
    Nothing -> error ("Couldn't find knowledge for agent: " ++ ag)

convert :: [(Agent, Projection)] -> S.Header -> [Knowledge] -> [Process]
convert [] _ _ = []
convert ((ag, pr):aprs) h kns = do
  let kn = getAgentKnowledge ag kns
  let p = initTranslate ag pr h kn
  let pp = breakProcess p
  let ps = convert aprs h kns
  (pp ++ ps)

-- getHonestAgentPicker :: Agent -> Agent -> S.MState AgentAction
-- getHonestAgentPicker actingAgent pickedAgent = do
--   state <- S.get
--   return (APickDomain actingAgent pickedAgent (S.hActors state))

-- getAllAgentPickers :: Agent -> [Agent] -> S.MState [AgentAction]
-- getAllAgentPickers _ [] = do
--   return []
-- getAllAgentPickers agent (a:as) = do
--   ap <- getAllAgentPicker agent a
--   aps <- getAllAgentPickers agent as
--   return (ap:aps)

-- getAllAgentPicker :: Agent -> Agent -> S.MState AgentAction
-- getAllAgentPicker actingAgent pickedAgent = do
--   state <- S.get
--   return (APickDomain actingAgent pickedAgent ((S.hActors state) ++ (S.dActors state)))

-- insertAgentPickers :: [[AgentAction]] -> S.MState [[AgentAction]]
-- insertAgentPickers ((a:as):aas) = do
--   let currentAgent = H.getAgentActionAgent a
--   agents <- S.getVariableAgents
--   hAgentPicker <- getHonestAgentPicker currentAgent currentAgent
--   dAgentPickers <- getAllAgentPickers currentAgent (agents List.\\ [currentAgent])
--   return ((a:hAgentPicker:dAgentPickers ++ as):aas)

initTranslate :: Agent -> Projection -> S.Header -> [Msg] -> Process
initTranslate ag pr h kn = do
  let s = S.addInitialState h kn S.initialState
  let (p, _) = translate ag pr s 1

  p
  -- TODO: generate agent picking code
  -- TODO: create S

translate :: Agent -> Projection -> S.State -> Int -> (Process, Int)
translate ag (Receive m r) s tn = do
  let (x, s1) = S.registerFresh m ToDo s
  let (chs, s2) = analyzeToDo s1
  let s3 = S.addToXVars x s2
  let (p, tn1) = translate ag r s3 tn
  let nestedChecks = foldr convertCheck p chs
  (NReceive x nestedChecks, tn1)
translate ag (Read v c t r) s tn = do
  let rs = compose t s
  let (x, s1) = S.registerFresh (Atom v) ToDo s
  let s2 = S.addToXVars x s1
  let (p, tn1) = translate ag r s2 tn
  (NRead x c (head rs) p, tn1)
translate ag (Choice m v d r) s tn = do
  let rrs = composeMany (map Atom d) s
  if any ([]==) rrs then
    error ("Could not compose some of the elements in domain: " ++ (show d))
  else do
    let (x, s1) = S.registerFresh (Atom v) ToDo s
    let s2 = S.addToXVars x s1
    let (p, tn1) = translate ag r s2 tn
    (NChoice m x d p, tn1)
translate ag (If f r1 r2) s tn = do
  let fr = composeFormula f s
  let (p1, tn1) = translate ag r1 s tn
  let (p2, tn2) = translate ag r2 s tn1
  (NIf fr p1 p2, tn2)
translate ag (New vars r) s tn = do
  let msgs = map (\v -> Atom v) vars
  let (ls, s1) = S.registerManyFresh msgs Done s
  let (p, tn1) = translate ag r s1 tn
  (NNew ls p, tn1)
translate ag (Send m r) s tn = do
  let rs = compose m s
  let (p, tn1) = translate ag r s tn
  (NSend (head rs) p, tn1)
translate ag (Write c t1 t2 r) s tn = do
  let rs1 = compose t1 s
  let rs2 = compose t2 s
  let (p, tn1) = translate ag r s tn
  (NWrite c (head rs1) (head rs2) p, tn1)
translate ag (Release m f r) s tn = do
  let fr = composeFormula f s
  let (p, tn1) = translate ag r s tn
  (NRelease m fr p, tn1)
translate ag (TxnEnd (Split r1 r2)) s tn =
  if r1 == Nil && r2 == Nil then
    (NNil, tn)
  else do
    let (s1, newTn, xs, ys) = prepStateForBreak tn s
    let (p1, tn1) = translate ag r1 s1 newTn
    let (p2, tn2) = translate ag r2 s1 tn1
    (endTxn ag xs newTn (NBreak (startTxn ag ys newTn p1) (startTxn ag ys newTn p2)), tn2)
translate ag (TxnEnd r) s tn =
  if r == Nil then
    (NNil, tn)
  else do
    let (s1, newTn, xs, ys) = prepStateForBreak tn s
    let (p, tn1) = translate ag r s1 newTn
    (endTxn ag xs newTn (NBreak (startTxn ag ys newTn p) NNil), tn1)
translate ag Nil _ tn = (NNil, tn)

prepStateForBreak :: Int -> S.State -> (S.State, Int, [String], [String])
prepStateForBreak tn s = do
  let xs = S.xVars s
  let newTn = tn + 1
  let ys = (S.yVars s) ++ xs
  let s1 = s { S.xVars = [], S.yVars = ys }
  (s1, newTn, xs, ys)

breakTxn :: Process -> (Process, [Process])
breakTxn (NReceive l p) = do
  let (p', rest) = breakTxn p
  (NReceive l p', rest)
breakTxn (NTry l r p1 p2) = do
  let (p1', rest1) = breakTxn p1
  let (p2', rest2) = breakTxn p2
  (NTry l r p1' p2', rest1 ++ rest2)
breakTxn (NIf f p1 p2) = do
  let (p1', rest1) = breakTxn p1
  let (p2', rest2) = breakTxn p2
  (NIf f p1' p2', rest1 ++ rest2)
breakTxn (NChoice m l d p) = do
  let (p', rest) = breakTxn p
  (NChoice m l d p', rest)
breakTxn (NRead l c r p) = do
  let (p', rest) = breakTxn p
  (NRead l c r p', rest)
breakTxn (NNew ls p) = do
  let (p', rest) = breakTxn p
  (NNew ls p', rest)
breakTxn (NSend r p) = do
  let (p', rest) = breakTxn p
  (NSend r p', rest)
breakTxn (NWrite c r1 r2 p) = do
  let (p', rest) = breakTxn p
  (NWrite c r1 r2 p', rest)
breakTxn (NRelease m f p) = do
  let (p', rest) = breakTxn p
  (NRelease m f p', rest)
breakTxn NNil =
  (NNil, [])
breakTxn (NBreak p NNil) =
  (NNil, [p])
breakTxn (NBreak NNil p) =
  (NNil, [p])
breakTxn (NBreak p1 p2) =
  (NNil, [p1, p2])

breakProcess :: Process -> [Process]
breakProcess p = do
  let (p', rest) = breakTxn p
  p':(concatMap breakProcess rest)
