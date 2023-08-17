module Analysis where

import Types (Msg (Atom, Comp), Agent, Recipe (RPub, RLabel, RComp), Action (Local, Comm, End), PProcess(PNew, PChoice, PIf, PRead, PRelease, PWrite), Mode (MStar),
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

filterRolesOnlyMsgs :: [Agent] -> [Msg] -> [Agent]
filterRolesOnlyMsgs roles [] = []
filterRolesOnlyMsgs roles (Atom x:msgs) =
  if List.elem x roles then
    (x:filterRolesOnlyMsgs roles msgs)
  else 
    filterRolesOnlyMsgs roles msgs
filterRolesOnlyMsgs roles (Comp _ _:msgs) =
  filterRolesOnlyMsgs roles msgs

getRolesFromKnowledge :: Agent -> S.Header -> [Agent]
getRolesFromKnowledge ag h = do
  let roles = Map.keys (S.roles h)
  let know = Map.findWithDefault [] ag (S.know h)
  filterRolesOnlyMsgs roles know

nestAgentPick :: [Agent] -> S.Header -> Projection -> Projection
nestAgentPick [] _ p = p
nestAgentPick (r:rs) h p = do
  let rest = nestAgentPick rs h p
  let domain = Map.findWithDefault [] r (S.roles h)
  Choice MStar r domain rest

initProject :: Agent -> Action -> S.Header -> Projection
initProject ag ac h = do
  let roles = getRolesFromKnowledge ag h
  let proj = project ag ac
  nestAgentPick roles h proj

actionsToProjs :: Action -> S.Header -> [(Agent, Projection)]
-- split actions into corresponding projections
actionsToProjs ac h = do
  let agents = H.getActors ac
  map (\ag -> (ag, (initProject ag ac h))) agents

getCells :: Action -> [CellDef]
getCells a = do
  let acs = List.nub (H.getActors a)
  let stepCells = map (\ac -> ("intSTEP_" ++ ac, Atom "intSID", Atom (ac ++ "0"))) acs
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
-- re-register all labels with Done marking
setAllLabelsDone [] _ s = s
setAllLabelsDone ((RLabel l):rs) msg s = do
  let s1 = setAllLabelsDone rs msg s
  S.register msg Done l s1
setAllLabelsDone (_:rs) msg s =
  setAllLabelsDone rs msg s

analyzeToDo :: S.State -> S.MCounter ([Check], S.State)
-- try to decomposes all messages in frame that are marked ToDo
analyzeToDo s = do
  case (List.find (\(_,(_,mark)) -> mark == ToDo) (Map.toList (S.frame s))) of
    Just (l,(msg,_)) -> do
      (ch, s1) <- analyze l msg s
      (chs, s2) <- analyzeToDo s1
      return ((ch ++ chs), s2)
    Nothing -> do return ([], s)
  
analyze :: Label -> Msg -> S.State -> S.MCounter ([Check], S.State)
-- pair decomposition
analyze l msg@(Comp "pair" (fr:sc:[])) s = do
  let s1 = S.register msg Done l s
  (l1, s2) <- S.registerFresh fr ToDo s1
  (l2, s3) <- S.registerFresh sc ToDo s2
  (chs, s4) <- analyzeToDo s3
  return (([CTry l1 (RComp "proj1" [RLabel l]), CTry l2 (RComp "proj2" [RLabel l])] ++ chs), s4)
-- scrypt decomposition, has to have a key sk in frame to receive the msg
analyze l msg@(Comp "scrypt" (sk:dm:r:[])) s = do
  let rs = compose sk s
  if rs /= [] then do
    let s1 = S.register msg Done l s
    (l1, s2) <- S.registerFresh dm ToDo s1
    (chs, s3) <- analyzeToDo s2
    return (([CTry l1 (RComp "dscrypt" [(head rs), RLabel l])] ++ chs), s3)
  else
    return ([], s)
-- crypt decomposition, has to have a private key inv(pk) in frame to receive the msg
analyze l msg@(Comp "crypt" (pk:dm:r:[])) s = do
  let rs = compose (Comp "inv" [pk]) s
  if rs /= [] then do
    let s1 = S.register msg Done l s
    (l1, s2) <- S.registerFresh dm ToDo s1
    (chs, s3) <- analyzeToDo s2
    return (([CTry l1 (RComp "dcrypt" [(head rs), RLabel l])] ++ chs), s3)
  else
    return ([], s)
-- sign decomposition, has to have a public key pk in frame to receive the msg
analyze l msg@(Comp "sign" ((Comp "inv" (pk:[])):dm:[])) s = do
  let rs = compose pk s
  if rs /= [] then do
    let s1 = S.register msg Done l s
    (l1, s2) <- S.registerFresh dm ToDo s1
    (chs, s3) <- analyzeToDo s2
    return (([CTry l1 (RComp "open" [(head rs), RLabel l])] ++ chs), s3)
  else
    return ([], s)
-- inv decomposition
analyze l msg@(Comp "inv" (pk:[])) s = do
  let rs = compose pk s
  if rs /= [] then do
    let s1 = S.register msg Done l s
    return (([CIf (BEq (head rs) (RComp "pubk" [RLabel l]))]), s1)
  else
    return ([], s)
-- default decomposition
analyze l msg s = do
  let rs = compose msg s
  if rs /= [] then do
    let ifChecks = map (\(a,b) -> CIf (BEq a b)) (getAllRecipePairs l rs)
    let s1 = setAllLabelsDone rs msg s
    return (ifChecks, s1)
  else
    return ([], s)
  
convertCheck :: Check -> Process -> Process
-- convert checks into processes
convertCheck (CTry l r) acc =
  NTry l r acc NNil
convertCheck (CIf cond) acc = do
  NIf cond acc NNil

getStepCell :: Agent -> String
getStepCell ag = "intSTEP_" ++ ag

getStepLabel :: Agent -> Int -> String
getStepLabel ag tn = "int" ++ ag ++ (show tn)

endTxn :: Agent -> [Label] -> Int -> Process -> S.MCounter Process
-- create the ending processes of a transaction
endTxn ag [] tn p = do
  let stepCell = getStepCell ag
  let stepLabel = getStepLabel ag tn
  S.addCell stepCell
  S.addPubLabel stepLabel
  return (NWrite stepCell (RPub "intSID") (RPub stepLabel) (NSend (RPub "intSID") p))
endTxn ag (x:xs) tn p = do
  let memCell = "intMEM_" ++ x
  S.addCell memCell
  end <- endTxn ag xs tn p
  return (NWrite memCell (RPub "intSID") (RLabel x) end)

startTxn :: Agent -> [Label] -> Int -> Process -> Process
-- create the starting processes of a transaction
startTxn ag ys tn p = do
  NReceive "intSID" (
    NRead "intSTEP" (getStepCell ag) (RPub "intSID") (
      NIf (BEq (RLabel "intSTEP") (RPub (getStepLabel ag tn))) (
        readAll ys p) NNil))

readAll :: [Label] -> Process -> Process
-- create the read processes for beginning of a transaction
readAll [] p = p
readAll (y:ys) p = NRead y ("intMEM_" ++ y) (RPub "intSID") p

initConvert :: [(Agent, Projection)] -> S.Header -> S.MCounter [Process]
initConvert aprs h = do
  (p:ps) <- convert aprs h
  return (NNew ["intSID"] (NSend (RPub "intSID") NNil):(NReceive ("intSID") p):ps) 

convert :: [(Agent, Projection)] -> S.Header -> S.MCounter [Process]
-- convert agent related projections to processes
convert [] _ = do return []
convert ((ag, pr):aprs) h = do
  p <- initTranslate ag pr h
  let pp = breakProcess p
  ps <- convert aprs h 
  return (pp ++ ps)

-- getHonestAgentChoice :: Agent -> S.Header -> Process -> Process
-- -- get a choice process for picking an honest agent
-- getHonestAgentChoice ag h r = NChoice MStar ag (S.hAgs h) r

-- getAllAgentChoice :: Agent -> S.Header -> Process -> Process
-- -- get a choice process for picking any agent
-- getAllAgentChoice ag h r = NChoice MStar ag ((S.hAgs h) ++ (S.dAgs h)) r

-- freshLabel :: Int -> (Label, Int)
--  -- generate a fresh label
-- freshLabel i = ("X" ++ show i, i + 1)

-- registerManyFresh :: [Msg] -> Marking -> Int -> State -> ([Label], Int, State)
-- -- generate a new label and register message with given marking
-- registerManyFresh [] _ i s = ([], i, s)
-- registerManyFresh (msg:msgs) marking i s = do
--   let (x, i1) = freshLabel i
--   let s1 = register msg marking x s
--   let (rest, i2, s2) = registerManyFresh msgs marking i1 s1
--   (label:rest, i2, s2)

initTranslate :: Agent -> Projection -> S.Header -> S.MCounter Process
-- initialize a translation of a projection into a process chain
initTranslate ag pr h = do
  let know = Map.findWithDefault [] ag (S.know h)
  let s = S.addInitialState ag h know S.initialState
  c <- S.get
  S.put c { S.txnCounter = 0 }
  translate ag pr s
  -- TODO: create S

translate :: Agent -> Projection -> S.State -> S.MCounter Process
-- translate a projection into a process chain
translate ag (Receive m r) s = do
  (x, s1) <- S.registerFresh m ToDo s
  (chs, s2) <- analyzeToDo s1
  p <- translate ag r s2
  let nestedChecks = foldr convertCheck p chs
  return (NReceive x nestedChecks)
translate ag (Read v c t r) s = do
  let rs = compose t s
  (x, s1) <- S.registerFresh (Atom v) ToDo s
  p <- translate ag r s1
  return (NRead x c (head rs) p)
translate ag (Choice m v d r) s = do
  let rrs = composeMany (map Atom d) s
  if any ([]==) rrs then
    error ("Could not compose some of the elements in domain: " ++ (show d))
  else do
    (x, s1) <- S.registerFresh (Atom v) ToDo s
    p <- translate ag r s1
    return (NChoice m x d p)
translate ag (If f r1 r2) s = do
  let fr = composeFormula f s
  p1 <- translate ag r1 s
  p2 <- translate ag r2 s
  return (NIf fr p1 p2)
translate ag (New vars r) s = do
  let msgs = map (\v -> Atom v) vars
  (ls, s1) <- S.registerManyFresh msgs Done s
  p <- translate ag r s1
  return (NNew ls p)
translate ag (Send m r) s = do
  let rs = compose m s
  p <- translate ag r s
  return (NSend (head rs) p)
translate ag (Write c t1 t2 r) s = do
  let rs1 = compose t1 s
  let rs2 = compose t2 s
  p <- translate ag r s
  return (NWrite c (head rs1) (head rs2) p)
translate ag (Release m f r) s = do
  let fr = composeFormula f s
  p <- translate ag r s
  return (NRelease m fr p)
translate ag (TxnEnd (Split r1 r2)) s =
  if r1 == Nil && r2 == Nil then
    return (NNil)
  else do
    let (s1, xs, ys) = prepStateForBreak s
    newTxnNo <- S.freshTxnNo
    p1 <- translate ag r1 s1
    p2 <- translate ag r2 s1
    endTxn ag xs newTxnNo (NBreak (startTxn ag ys newTxnNo p1) (startTxn ag ys newTxnNo p2))
translate ag (TxnEnd r) s =
  if r == Nil then
    return (NNil)
  else do
    let (s1, xs, ys) = prepStateForBreak s
    newTxnNo <- S.freshTxnNo
    p <- translate ag r s1
    endTxn ag xs newTxnNo (NBreak (startTxn ag ys newTxnNo p) NNil)
translate ag Nil _ = return NNil

prepStateForBreak :: S.State -> (S.State, [String], [String])
-- reset a state for new transaction
prepStateForBreak s = do
  let xs = S.xVars s
  let ys = (S.yVars s) ++ xs
  let s1 = s { S.xVars = [], S.yVars = ys }
  (s1, xs, ys)

breakTxn :: Process -> (Process, [Process])
-- break process chain into one transaction and the test of the chain
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
-- break a process chain into tranactions 
breakProcess p = do
  let (p', rest) = breakTxn p
  p':(concatMap breakProcess rest)

updateHeaderAndCells :: S.Counter -> S.Header -> [CellDef] -> (S.Header, [CellDef])
updateHeaderAndCells c h cells = do
  let newToSig0 = ("intFRESH":(S.pubLabels c)) ++ (S.hAgs h) ++ (S.dAgs h)
  let newHeader = S.addToSigma0 newToSig0 h
  let newCells = map (\cell -> (cell, Atom "intSID", Atom "intFRESH")) (List.nub (S.cells c))
  (newHeader, newCells ++ cells)