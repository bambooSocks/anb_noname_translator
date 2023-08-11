module Analysis where

import ParserModel (Action(Local, Comm, End), PProcess(PNew, PChoice, PIf, PRead, PRelease, PWrite))
import Types (Mode, Msg(Atom, Comp), Agent, Recipe(RPub, RLabel, RComp), Mode(MStar), 
              Label, Marking(Done, ToDo), Var, Const, Cell, Formula(BEq, BAnd, BNot, BTrue),
              Projection(Receive, Send, New, Choice, Read, Write, Release, If, Split, TxnEnd, Nil),
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
    Split subProj1 subProj2
getProjection ag (Comm ag1 ag2 m rest) =
  if ag == ag1 then do
    let subProj = getProjection ag rest
    Send m (TxnEnd subProj)
  else if ag == ag2 then do
    let subProj = getProjection ag rest
    Receive m subProj
  else
    getProjection ag rest
getProjection ag _ =
  error ("Failed when retrieving projections for agent " ++ ag)

actionsToProjs :: Action -> [(Agent, Projection)]
-- split actions into corresponding projections
actionsToProjs as = do
  let agents = H.getActionAgents as
  map (\ag -> (ag, (getProjection ag as))) agents

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
    error ("Could not find a recipe for element in formula " ++ (H.formulaToStr f))
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

getAllRecipePairs :: [Recipe] -> [(Recipe,Recipe)]
getAllRecipePairs [] = 
  []
getAllRecipePairs (r:rs) = do
  let pairs = map (\rr -> (r,rr)) rs
  pairs ++ (getAllRecipePairs rs)

analyzeToDo :: S.State -> ([Check], S.State)
-- try to decomposes all messages in frame that are marked ToDo
analyzeToDo s = do
  analyzeFrame (Map.toList (S.frame s)) s

analyzeFrame :: [(Label,(Msg,Marking))] -> S.State -> ([Check], S.State)
-- traverses through the agent's frame and analyzes all ToDo entries
analyzeFrame [] s =
  ([],s)
analyzeFrame ((l,(msg,mark)):rest) s = do
  if mark == ToDo then do
    let (ch, s1) = analyze l msg s
    let (chs, s2) = analyzeFrame rest s1
    ((ch ++ chs), s2)
  else
    analyzeFrame rest s
  
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
analyze l msg s = do --TODO: verify this works TEST ME!!!!
  let rs = compose msg s
  let ifChecks = map (\(a,b) -> CIf (BEq a b)) (getAllRecipePairs rs)
  let s1 = S.register msg Done l s
  (ifChecks, s1)
  
convertCheck :: Check -> Process -> Process
-- convert checks into processes
convertCheck (CTry l r) acc =
  NTry l r acc NNil
convertCheck (CIf cond) acc = do
  NIf cond acc NNil

endTxn :: Agent -> [Label] -> String -> Process -> Process
endTxn ag [] tn p = do
  NWrite ("__INT_STEP_" ++ ag) (RPub "S") (RPub tn) (NSend (RPub "S") p)
endTxn ag (x:xs) tn p = do
  NWrite ("__INT_MEM_" ++ x) (RPub "S") (RLabel x) (endTxn ag xs tn p)

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
  | NBreak Process Process
  deriving (Show)

-- initTranslate :: Agent -> Projection -> S.MState Process
-- initTranslate ag p = do
--   -- set state for step counter to 0
--   -- generate agent picking code
--   -- create S
--   let lp = Choice 
--   translate ag p

translate :: Agent -> Projection -> S.State -> Process
translate ag (Receive m r) s = do
  let (x, s1) = S.registerFresh m ToDo s
  let (chs, s2) = analyzeToDo s1
  let s3 = S.addToXVars x s2
  let p = translate ag r s3
  let nestedChecks = foldr convertCheck p chs
  NReceive x nestedChecks -- TODO: test me
translate ag (Read v c t r) s = do
  let rs = compose t s
  let (x, s1) = S.registerFresh (Atom v) ToDo s
  let s2 = S.addToXVars x s1
  NRead x c (head rs) (translate ag r s2)
translate ag (Choice m v d r) s = do
  let rrs = composeMany (map Atom d) s -- TODO: test me
  let (x, s1) = S.registerFresh (Atom v) ToDo s
  let s2 = S.addToXVars x s1
  NChoice m x d (translate ag r s2)
translate ag (If f r1 r2) s = do
  let fr = composeFormula f s
  NIf fr (translate ag r1 s) (translate ag r2 s)
translate ag (New vars r) s = do
  let msgs = map (\v -> Atom v) vars
  let (ls, s1) = S.registerManyFresh msgs Done s
  NNew ls (translate ag r s1)
translate ag (Send m r) s = do
  let rs = compose m s
  NSend (head rs) (translate ag r s)
translate ag (Write c t1 t2 r) s = do
  let rs1 = compose t1 s
  let rs2 = compose t2 s
  let p = translate ag r s
  NWrite c (head rs1) (head rs2) p
translate ag (Release m f r) s = do
  let fr = composeFormula f s
  let p = translate ag r s
  NRelease m fr p
translate ag (TxnEnd (Split r1 r2)) s = do
  let xs = S.xVars s
  let (tn, s1) = S.freshTxnName ag s
  let s2 = s1 { S.xVars = [], S.yVars = ((S.yVars s1) ++ xs) }
  let p1 = translate ag r1 s2
  let p2 = translate ag r2 s2
  endTxn ag xs tn (NBreak p1 p2)
translate ag (TxnEnd r) s = do
  let xs = S.xVars s
  let (tn, s1) = S.freshTxnName ag s
  let s2 = s1 { S.xVars = [], S.yVars = ((S.yVars s1) ++ xs) }
  let p = translate ag r s2
  endTxn ag (S.xVars s) tn (NBreak p NNil)
translate ag Nil _ = NNil

-- TODO: update the document to be up to date


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
breakTxn (NBreak p1 p2) =
  (NNil, [p1, p2])

breakProcess :: Process -> [Process]
breakProcess p = do
  let (p', rest) = breakTxn p
  p':(concatMap breakProcess rest)

initialHeader :: S.Header
initialHeader = S.Header 
  { S.sigma0     = [("ok", 0), ("wrong", 0), ("res1", 0), ("res2", 0)]
  , S.sigma      = [("h", 2)]
  , S.sigmaPriv  = []
  , S.agents     = ["a","b"]
  , S.agentsDish = ["i"]
  }

main :: IO ()
main = do
  let initialKnowledge = [Atom "A", Atom "B"]
  let actions = Local "A" (PChoice MStar "X" ["a", "b"]) (Comm "A" "B" (Atom "X") (Local "B" (PIf (BEq (Atom "X") (Atom "a")) (Comm "B" "A" (Atom "ok") (Comm "A" "B" (Atom "res1") (Comm "B" "A" (Atom "ok") End))) (Comm "B" "A" (Atom "wrong") (Comm "A" "B" (Atom "res2") (Local "B" (PIf (BEq (Atom "X") (Atom "b")) (Comm "B" "A" (Atom "ok") End) (Comm "B" "A" (Atom "wrong") End)) End)))) End))
  
  let projs = actionsToProjs actions
  let (ag,proj):_ = projs

  -- putStrLn $ show proj

  let s = S.addInitialState initialHeader initialKnowledge S.initialState
  let p = translate ag proj s

  let ps = breakProcess p

  putStrLn $ show ps
