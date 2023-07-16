module Analysis where

import ParserModel (
  Action(Local, Comm, PIf), 
  Var, Const, Cell, 
  PProcess(PNew, PChoice, PRead, PWrite, PRelease, PSend, PReceive))
import Types (Mode, Formula (BEq), Msg(Atom, Comp), Agent, Recipe(RAtom), Mode(MStar))
import qualified Helper as H
import qualified Data.List as List
import qualified Data.Map as Map

type TxnID = String -- used to label each of the transactions
type Role = String -- temp

data Projection
  = Receive Msg Projection
  | Read Var Cell Msg Projection
  | Choice Mode Var [Const] Projection
  | If Formula Projection Projection
  | New [Var] Projection
  | Send Msg Projection
  | Write Cell Msg Msg Projection
  | Release Mode Formula Projection
  | TxnEnd Projection
  | NonDetSplit Projection Projection
  | Nil
  deriving (Show)

getProjection :: Agent -> [Action] -> Projection
getProjection ag [] =
  Nil
getProjection ag ((Local ag' (PNew vars)):as) =
  if ag == ag' then do
    let subProcess = getProjection ag as
    New vars subProcess
  else
    getProjection ag as
getProjection ag ((Local ag' (PChoice m v cs)):as) =
  if ag == ag' then do
    let subProcess = getProjection ag as
    Choice m v cs subProcess
  else
    getProjection ag as
getProjection ag ((Local ag' (PRead v c m)):as) =
  if ag == ag' then do
    let subProcess = getProjection ag as
    Read v c m subProcess
  else
    getProjection ag as
getProjection ag ((Local ag' (PWrite c m1 m2)):as) =
  if ag == ag' then do
    let subProcess = getProjection ag as
    Write c m1 m2 subProcess
  else
    getProjection ag as
getProjection ag ((Local ag' (PRelease m f)):as) =
  if ag == ag' then do
    let subProcess = getProjection ag as
    Release m f subProcess
  else
    getProjection ag as
getProjection ag ((Comm ag1 ag2 m):as) =
  if ag == ag1 then do
    let subProcess = getProjection ag as
    Send m (TxnEnd subProcess)
  else if ag == ag2 then do
    let subProcess = getProjection ag as
    Receive m subProcess
  else
    getProjection ag as
getProjection ag ((PIf ag' f as1 as2):[]) =
  if ag == ag' then do
    let subProcess1 = getProjection ag as1
    let subProcess2 = getProjection ag as2
    If f subProcess1 subProcess2
  else do
    let subProcess1 = getProjection ag as1
    let subProcess2 = getProjection ag as2
    NonDetSplit subProcess1 subProcess2
getProjection ag _ =
  error ("Failed when retrieving projections for agent " ++ ag)

getActionsAgents :: [Action] -> [Agent]
getActionsAgents as = List.nub (concatMap getActionAgents as)

getActionAgents :: Action -> [Agent]
-- get all agents related to an action
getActionAgents (Local agent _) = [agent]
getActionAgents (Comm agent1 agent2 _) = [agent1, agent2]
getActionAgents (PIf agent _ as1 as2) = List.nub ([agent] ++ (getActionsAgents as1) ++ (getActionsAgents as2))

actionsToProjs :: [Action] -> [(Agent, Projection)]
-- split actions into corresponding projections
actionsToProjs as = do
  let agents = getActionsAgents as
  map (\ag -> (ag, (getProjection ag as))) agents

main :: IO ()
main = do
  -- let actions = [Local "A" (PNew ["N1", "N2"]), Comm "A" "B" (Comp "pair" [Atom "N1", Comp "h" [Atom "N1", Atom "N2"]]), Comm "B" "A" (Atom "ok"), Comm "A" "B" (Atom "N2"), Comm "B" "A" (Atom "ok")]
  let actions = [Local "A" (PChoice MStar "X" ["a", "b"]), Comm "A" "B" (Atom "X"), PIf "B" (BEq (RAtom "X") (RAtom "a")) [Comm "B" "A" (Atom "ok"),Comm "A" "B" (Atom "res1"),Comm "B" "A" (Atom "ok")] [Comm "B" "A" (Atom "wrong"),Comm "A" "B" (Atom "res2"),PIf "B" (BEq (RAtom "X") (RAtom "b")) [Comm "B" "A" (Atom "ok")] [Comm "B" "A" (Atom "wrong")]]]
  
  let projs = actionsToProjs actions

  putStrLn $ show projs
