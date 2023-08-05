module Analysis where

import ParserModel (Action(Local, Comm, End), PProcess(PNew, PChoice, PIf, PRead, PRelease, PWrite))
import Types (Mode, Msg(Atom, Comp), Agent, Recipe(RPub, RLabel, RComp), Mode(MStar), 
              Label, Marking(Done, ToDo), Var, Const, Cell, Formula(BEq, BAnd, BNot, BTrue),
              Projection(Receive, Send, New, Choice, Read, Write, Release, If, Split, TxnEnd, TxnBegin, Nil),
              Check(CTry, CIf))
import qualified Helper as H
import qualified Data.List as List
import qualified Data.Map as Map
import qualified State as S

type TxnID = String -- used to label each of the transactions
type Role = String -- temp

getProjection :: Agent -> Action -> Projection
getProjection ag End =
  Nil
getProjection ag (Local ag' (PNew vars) rest) =
  if ag == ag' then do
    let subProj = getProjection ag rest
    New vars subProj
  else
    getProjection ag rest
getProjection ag (Local ag' (PChoice m v cs) rest) =
  if ag == ag' then do
    let subProj = getProjection ag rest
    Choice m v cs subProj
  else
    getProjection ag rest
getProjection ag (Local ag' (PRead v c m) rest) =
  if ag == ag' then do
    let subProj = getProjection ag rest
    Read v c m subProj
  else
    getProjection ag rest
getProjection ag (Local ag' (PWrite c m1 m2) rest) =
  if ag == ag' then do
    let subProj = getProjection ag rest
    Write c m1 m2 subProj
  else
    getProjection ag rest
getProjection ag (Local ag' (PRelease m f) rest) =
  if ag == ag' then do
    let subProj = getProjection ag rest
    Release m f subProj
  else
    getProjection ag rest
getProjection ag (Local ag' (PIf f rest1 rest2) End) =
  if ag == ag' then do
    let subProj1 = getProjection ag rest1
    let subProj2 = getProjection ag rest2
    If f subProj1 subProj2
  else do
    let subProj1 = getProjection ag rest1
    let subProj2 = getProjection ag rest2
    NonDetSplit subProj1 subProj2
getProjection ag (Comm ag1 ag2 m rest) =
  if ag == ag1 then do
    let subProj = getProjection ag rest
    Send m (TxnEnd subProj)
  else if ag == ag2 then do
    let subProj = getProjection ag rest
    TxnBegin (Receive m subProj)
  else
    getProjection ag rest
getProjection ag _ =
  error ("Failed when retrieving projections for agent " ++ ag)

annotateTransitions :: Projection -> Int -> (Int, Projection)
annotateTransitions (TxnEnd _ (TxnBegin _ rest)) n = do
  let incN = n + 1
  let (newN, subProj) = annotateTransitions rest incN
  (newN, (TxnEnd incN (TxnBegin incN subProj)))
annotateTransitions (TxnEnd _ (Split (TxnBegin _ rest1) (TxnBegin _ rest2))) n = do
  let incN = n + 1
  let (newN1, subProj1) = annotateTransitions rest1 incN
  let (newN2, subProj2) = annotateTransitions rest2 newN1
  (newN, (TxnEnd incN (Split (TxnBegin incN rest1) (TxnBegin incN rest2))))
annotateTransitions p n = do
  let incN = n + 1
  let (newN, subProj) = annotateTransitions rest incN
  (newN, (TxnEnd incN (TxnBegin incN subProj)))


actionsToProjs :: Action -> [(Agent, Projection)]
-- split actions into corresponding projections
actionsToProjs as = do
  let agents = H.getActionAgents as
  map (\ag -> (ag, (getProjection ag as))) agents

composeFormula :: Agent -> Formula Msg -> S.MState (Formula Recipe)
-- convert message formula to recipe formula
composeFormula ag (BAnd f1 f2) = do
  cf1 <- composeFormula ag f1
  cf2 <- composeFormula ag f2
  return (BAnd cf1 cf2)
composeFormula ag (BNot f) = do
  cf <- composeFormula ag f
  return (BNot cf)
composeFormula ag BTrue =
  return BTrue
composeFormula ag f@(BEq m1 m2) = do
  r1 <- compose ag m1
  r2 <- compose ag m2
  if r1 == [] || r2 == [] then
    error ("Could not find a recipe for element in formula " ++ (H.formulaToStr f))
  else
    return (BEq (head r1) (head r2))

getLabelsInFrame :: Agent -> Msg -> S.MState [Recipe]
-- get all labels in frame
getLabelsInFrame ag msg = do
  frame <- S.getFrameForAgent ag
  return (map (\l -> RLabel l) (Map.keys (Map.filter (\(m,_) -> m == msg) frame)))

composeMany :: Agent -> [Msg] -> S.MState [[Recipe]]
-- get recipes for several messages
composeMany ag [] = do
  return []
composeMany ag (msg:msgs) = do
  r <- compose ag msg
  rs <- composeMany ag msgs
  return (r:rs)

compose :: Agent -> Msg -> S.MState [Recipe]
-- get recipes for a message
compose ag msg@(Atom x) = do
  labelsInFrame <- getLabelsInFrame ag msg
  isPub <- S.isPublicId x 0
  if isPub then
    return ((RPub x):labelsInFrame)
  else
    return labelsInFrame
compose ag msg@(Comp x args) = do
  labelsInFrame <- getLabelsInFrame ag msg
  isPub <- S.isPublicId x (length args)
  rs <- composeMany ag args
  let argrs = map head rs -- TODO: check whether each evaluated and get all combinations
  if isPub then
    return ((RComp x argrs):labelsInFrame) 
  else do
    return labelsInFrame

getAllRecipePairs :: [Recipe] -> [(Recipe,Recipe)]
getAllRecipePairs [] = 
  []
getAllRecipePairs (r:rs) = do
  let pairs = map (\rr -> (r,rr)) rs
  pairs ++ (getAllRecipePairs rs)

freshLabel :: S.MState Label
-- generate a fresh label
freshLabel = do
  state <- S.get
  let i = S.counter state
  let label = "X" ++ show i
  S.put state { S.counter = i + 1 }
  return label

freshTxnName :: Agent -> S.MState Msg
-- generate a fresh label
freshTxnName ag = do
  state <- S.get
  let i = S.txnCounter state
  let label = ag ++ "_" ++ show i
  S.put state { S.txnCounter = i + 1 }
  return (Atom label)

register :: Agent -> Msg -> Marking -> Label -> S.MState ()
-- register a new label and corresponding message in the frame
register agent msg marking label = do
  frame <- S.getFrameForAgent agent
  let newFrame = Map.insert label (msg, marking) frame
  S.putFrameForAgent agent newFrame

registerFresh :: Agent -> Msg -> Marking -> S.MState Label
-- generate a new label and register message with given marking
registerFresh agent msg marking = do
  label <- freshLabel
  register agent msg marking label
  return label

registerManyFresh :: Agent -> [Msg] -> Marking -> S.MState [Label]
-- generate a new label and register message with given marking
registerManyFresh _ [] _ = do
  []
registerManyFresh ag msg:msgs marking = do
  label <- freshLabel
  register ag msg marking label
  return (label:(registerManyFresh ag msgs marking))

analyzeToDo :: Agent -> S.MState [Check]
-- try to decomposes all messages in frame that are marked ToDo
analyzeToDo ag = do
  frame <- S.getFrameForAgent ag
  analyzeFrame ag (Map.toList frame)

analyzeFrame :: Agent -> [(Label,(Msg,Marking))] -> S.MState [Check]
-- traverses through the agent's frame and analyzes all ToDo entries
analyzeFrame ag [] = do
  return []
analyzeFrame ag ((l,(msg,mark)):rest) = do
  chs <- analyzeFrame ag rest
  if mark == ToDo then do
    ch <- analyze ag l msg
    return (ch ++ chs)
  else
    return chs
  
analyze :: Agent -> Label -> Msg -> S.MState [Check]
-- pair decomposition
analyze ag l msg@(Comp "pair" (f:s:[])) = do
  register ag msg Done l
  l1 <- registerFresh ag f ToDo
  l2 <- registerFresh ag s ToDo
  chs <- analyzeToDo ag
  return ([CTry l1 (RComp "proj1" [RLabel l]), CTry l2 (RComp "proj2" [RLabel l])] ++ chs)
-- scrypt decomposition, has to have a key sk in frame to receive the msg
analyze ag l msg@(Comp "scrypt" (sk:dm:r:[])) = do
  rs <- compose ag sk
  if rs /= [] then do
    register ag msg Done l
    l1 <- registerFresh ag dm ToDo
    chs <- analyzeToDo ag
    return ([CTry l1 (RComp "dscrypt" [(head rs), RLabel l])] ++ chs)
  else
    return []
-- crypt decomposition, has to have a private key inv(pk) in frame to receive the msg
analyze ag l msg@(Comp "crypt" (pk:dm:r:[])) = do
  rs <- compose ag (Comp "inv" [pk])
  if rs /= [] then do
    register ag msg Done l
    l1 <- registerFresh ag dm ToDo
    chs <- analyzeToDo ag
    return ([CTry l1 (RComp "dcrypt" [(head rs), RLabel l])] ++ chs)
  else
    return []
-- sign decomposition, has to have a public key pk in frame to receive the msg
analyze ag l msg@(Comp "sign" ((Comp "inv" (pk:[])):dm:[])) = do
  rs <- compose ag pk
  if rs /= [] then do
    register ag msg Done l
    l1 <- registerFresh ag dm ToDo
    chs <- analyzeToDo ag
    return ([CTry l1 (RComp "open" [(head rs), RLabel l])] ++ chs)
  else
    return []
-- inv decomposition
analyze ag l msg@(Comp "inv" (pk:[])) = do
  rs <- compose ag pk
  if rs /= [] then do
    register ag msg Done l
    return ([CIf (BEq (head rs) (RComp "pubk" [RLabel l]))])
  else
    return []
-- default decomposition
analyze ag l msg = do --TODO: verify this works TEST ME!!!!
  rs <- compose ag msg
  let ifChecks = map (\(a,b) -> CIf (BEq a b)) (getAllRecipePairs rs)
  register ag msg Done l
  return ifChecks
  
convertCheck :: Check -> Process -> Process
-- convert checks into processes
convertCheck (CTry l r) acc =
  NTry l r acc NNil
convertCheck (CIf cond) acc = do
  NIf cond acc NNil

endTxn :: [Label] -> String -> Process
endTxn [] tn = do
  NWrite ("__INT_STEP_" ++ ag) (RAtom "S") tn (NSend (RAtom "S") NNil)
endTxn x:xs tn = do
  NWrite ("__INT_MEM_" ++ x) (RAtom "S") (RAtom x) (endTxn xs tn)

data Process
  = NReceive Label Process -- receive(label)
  | NTry Label Recipe Process Process -- try check in process catch process
  | NIf (Formula Recipe) Process Process -- if formula then processes else processes
  | NChoice Mode Label [Label] Process -- */<> m in {m1, m2, m3, ...}
  | NRead Label Cell Recipe Process -- label := cell[msg]
  | NNew [Label] Process -- new s1,s2
  | NSend Recipe Process -- send(msg)
  | NWrite Cell Recipe Recipe Process -- cell[msg] := msg
  | NRelease Mode (Formula Recipe) Process -- */<> formula
  | NNil
  | NBreak Process
  deriving (Show)

-- initProj :: Projection -> S.MState [LProcess]
-- initProj p = do
--   -- set state for step counter to 0
--   -- generate agent picking code
--   -- create S
--   let lp = Choice 
--   translateProj p [lp]



translateProj :: Agent -> Projection -> S.MState Process
translateProj ag (Receive m r) = do
  l <- registerFresh ag m ToDo
  chs <- analyzeToDo ag
  S.addToXVars l
  tr <- translateProj r
  let nestedChecks = foldr convertCheck tr chs
  NReceive l nestedChecks -- TODO: test me
translateProj ag (Read v c t r) = do
  l <- registerFresh ag (Atom v) ToDo
  rs <- compose ag t
  S.addToXVars l
  tr <- translateProj rest
  NRead l c (head rs) tr
translateProj ag (Choice m v d r) = do
  l <- registerFresh ag m ToDo
  rrs <- composeMany ag (map Atom d) -- TODO: test me
  if any ([]==) rrs then
    error "Some of the elements from domain: " ++ (show d) ++ " cannot be composed"
  else
    S.addToXVars l
    tr <- translateProj r
    NChoice m l d tr
translateProj ag (If f r1 r2) = do
  fr <- composeFormula ag f
  tr1 <- translateProj r1
  tr2 <- translateProj r2
  NIf fr 
translateProj ag (New vars r) = do
  let msgs = map (\v -> Atom v) vars
  ls <- registerManyFresh ag msgs Done
  tr <- translateProj r
  NNew ls tr
translateProj ag (Send m r) = do
  rs <- compose ag m
  tr <- translateProj r
  NSend (head rs) tr
translateProj ag (Write c t1 t2 r) = do
  rs1 <- compose ag t1
  rs2 <- compose ag t2
  tr <- translateProj r
  NWrite c (head rs1) (head rs2) tr
translateProj ag (Release m f r) = do
  fr <- composeFormula ag f
  tr <- translateProj r
  NRelease m fr tr
translateProj ag (TxnEnd (Split (TxnBegin r1) (TxnBegin r2))) = do
  xVars <- S.getXVars
  addManyToYVars xVars
  clearXVars
  tn <- freshTxnName ag
  tr <- translateProj r1
  NWrite ("__INT_STEP_" ++ ag) (Atom "S") tn ()
translateProj ag (TxnEnd (TxnBegin r)) = do
  xVars <- S.getXVars
  addManyToYVars xVars
  clearXVars
  tn <- freshTxnName ag
  tr <- translateProj r
  endTxn xVars tn
translateProj ag Nil = do
  NNil




-- TODO: update the document to be up to date


main :: IO ()
main = do
  -- let actions = [Local "A" (PNew ["N1", "N2"]), Comm "A" "B" (Comp "pair" [Atom "N1", Comp "h" [Atom "N1", Atom "N2"]]), Comm "B" "A" (Atom "ok"), Comm "A" "B" (Atom "N2"), Comm "B" "A" (Atom "ok")]
  let actions = Local "A" (PChoice MStar "X" ["a", "b"]) (Comm "A" "B" (Atom "X") (Local "B" (PIf (BEq (Atom "X") (Atom "a")) (Comm "B" "A" (Atom "ok") (Comm "A" "B" (Atom "res1") (Comm "B" "A" (Atom "ok") End))) (Comm "B" "A" (Atom "wrong") (Comm "A" "B" (Atom "res2") (Local "B" (PIf (BEq (Atom "X") (Atom "b")) (Comm "B" "A" (Atom "ok") End) (Comm "B" "A" (Atom "wrong") End)) End)))) End))
  
  let projs = actionsToProjs actions

  let st:[] = S.execMState 
                (S.addInitialState 
                  0 -- TODO: add transaction number here 
                  [("ok", 0), ("wrong", 0)] 
                  [("h", 2)] 
                  []
                  ["A","B"]
                  ["a","b"]
                  ["i"]) S.initialState

  let st2 = S.evalMState (compose "A" (Comp "pair" [Atom "ok", Atom "wrong"])) st

  putStrLn $ show st2

  putStrLn $ show projs
