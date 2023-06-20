module Generator where

import Types (
  Msg (Atom, Comp), 
  Agent, Label, Check(CTry, CIf), 
  LocalAction (LNew, LPickDomain, LRead, LWrite, LRelease), 
  NNProcess (PReceive, PSend, PTry, PIf, PCheckIf, PNew, PPickDomain, PRead, PWrite, PRelease, PNil), 
  NNTransaction, NNCell,
  Formula(BNot, BAnd, BEq, BTrue),
  Recipe(RAtom, RComp),
  Mode(MStar, MDiamond),
  unableErrorMsg, msgToStr, recipeToStr, formulaToStr)

import qualified Verification as V
import Data.Foldable (for_)
import Data.List (intercalate)

-- data Action
--   = Local Agent LocalAction
--   | Comm Agent Agent Msg
--   deriving (Show)

convertLocalActionToProcess :: Agent -> LocalAction -> NNProcess
-- convert local action into a process
convertLocalActionToProcess agent (LNew ns) = PNew agent ns
convertLocalActionToProcess agent (LPickDomain x d) = PPickDomain agent x d
convertLocalActionToProcess agent (LRead l c m) = PRead agent l c m
convertLocalActionToProcess agent (LWrite c m1 m2) = PWrite agent c m1 m2
convertLocalActionToProcess agent (LRelease m f) = PRelease agent m f

splitActionsForNewSession :: [Action] -> [NNTransaction]
splitActionsForNewSession actions =
  splitActions actions [[PNew "_" ["S"]]]

splitActions :: [Action] -> [NNTransaction] -> [NNTransaction]
-- split actions into corresponding transactions
splitActions [] (t:ts) = do
  let tts = ((t ++ [PNil]):ts)
  reverse tts
splitActions ((Local agent ua):as) (t:ts) = do
  let p = convertLocalActionToProcess agent ua
  let newT = t ++ [p]
  splitActions as (newT:ts)
splitActions ((Comm agent1 agent2 m):as) (t:ts) = do
  let newT = t ++ [PSend agent1 m, PNil]
  let newTS = [PReceive agent2 "X" m]:newT:ts
  splitActions as newTS

checkTransactions :: [NNTransaction] -> V.MState [NNTransaction]
-- check that multiple transactions can be run
checkTransactions [] = do
  return []
checkTransactions (t:ts) = do
  tt <- checkTransaction t
  tts <- checkTransactions ts
  return (tt:tts)

convertCheck :: Agent -> Check -> [NNProcess] -> NNProcess
convertCheck agent (CTry c) acc =
  PTry agent c acc
convertCheck agent (CIf c) acc = do
  PCheckIf agent c acc

checkTransaction :: NNTransaction -> V.MState NNTransaction
-- check that singe transaction can be run
checkTransaction [] = do
  return []
-- special case for receiving process, when the following processes have to be nested
checkTransaction (p@(PReceive agent label msg):ps) = do
  (newLabel, checks) <- V.receive agent msg
  ts <- checkTransaction ps
  let nestedChecks = foldr (\ c acc -> [convertCheck agent c acc]) ts checks 
  return [
    PReceive agent newLabel msg, 
    PReceive agent "S" (Atom "_"), 
    PRead agent "T" "sessionTxn" (Atom "S"),
    PIf agent (BEq (RAtom "T") (RAtom "Tx")) nestedChecks ]
checkTransaction (p:ps) = do
  t <- checkProcess p
  ts <- checkTransaction ps
  return (t ++ ts)
  
checkProcess :: NNProcess -> V.MState NNTransaction
-- new nonce process checker
checkProcess p@(PNew agent xs) = do
  for_ xs (\x -> V.registerFresh agent (Atom x) V.ToDo)
  return [p]
-- send process checker
checkProcess p@(PSend agent msg) = do
  result <- V.canDeduceFromFrame agent msg
  if result then
    return [p, (PSend agent (Atom "S"))]
  else
    error (unableErrorMsg msg)
checkProcess p@(PPickDomain agent _ domain) = do
  result <- V.canDeduceManyFromFrame agent domain -- TODO: check also the sigmas for domain
  if result then
    return [p]
  else
    error "Unable to deduce a recipe for domain pick process from agent's frame"
checkProcess p@(PWrite agent c m1 m2) = do
  res1 <- V.canDeduceFromFrame agent m1
  res2 <- V.canDeduceFromFrame agent m2
  if res1 && res2 then
    return [p]
  else
    error "Unable to deduce a recipe for a write process from agent's frame"
-- default process checker
checkProcess p = do
  return [p]

generateCells :: [NNCell] -> String
generateCells cells = do
  let cellsStr = intercalate "\n" (map generateCell cells)
  "Cells:\n" ++ cellsStr ++ "\n"

generateCell :: NNCell -> String
generateCell (cell, index, initVal) =
  cell ++ "[" ++ (msgToStr index) ++ "] := " ++ (msgToStr initVal) 

generateTransactions :: Int -> [NNTransaction] -> String
generateTransactions _ [] = 
  ""
generateTransactions i (t:ts) =
  (generateTransaction ("T" ++ show i) t) ++ (generateTransactions (i+1) ts)

generateTransaction :: String -> NNTransaction -> String
generateTransaction name ps = do
  let processes = concatMap generateProcess ps
  "Transaction " ++ name ++ ":\n" ++ processes ++ "\n\n"

generateProcess :: NNProcess -> String
generateProcess (PNew _ xs) =
  "new(" ++ (intercalate "," xs) ++ ").\n"
generateProcess (PSend _ msg) =
  "send(" ++ (msgToStr msg) ++ ").\n"
generateProcess (PReceive _ l _) =
  "receive(" ++ l ++ ").\n"
generateProcess (PTry _ (l,r) ps) = do
  let subProcesses = concatMap generateProcess ps
  "try " ++ l ++ "=" ++ (recipeToStr r) ++ " in\n" ++ subProcesses ++ "\ncatch nil"
generateProcess (PCheckIf _ (l,r) ps) = do
  let subProcesses = concatMap generateProcess ps
  "if " ++ l ++ "=" ++ (recipeToStr r) ++ " then\n" ++ subProcesses ++ "\nelse nil"
generateProcess (PIf _ f ps) = do
  let subProcesses = concatMap generateProcess ps
  "if " ++ (formulaToStr f) ++ " then\n" ++ subProcesses ++ "\nelse nil"
generateProcess (PPickDomain _ m ms) = do
  let domain = map msgToStr ms
  "* " ++ (msgToStr m) ++ "in {" ++ (intercalate "," domain) ++ "}.\n"
generateProcess (PRead _ label cell msg) =
  label ++ " := " ++ cell ++ "[" ++ (msgToStr msg) ++ "].\n"
generateProcess (PWrite _ cell msg1 msg2) =
  cell ++ "[" ++ (msgToStr msg1) ++ "] := " ++ (msgToStr msg2) ++ ".\n"
generateProcess (PRelease _ MStar f) = 
  "* " ++ (formulaToStr f) ++ ".\n"
generateProcess (PRelease _ MDiamond f) = 
  "<> " ++ (formulaToStr f) ++ ".\n"
generateProcess PNil =
  "nil"

main :: IO ()
main = do
  -- let actions = [ Local "A" (LNew ["N1"]), Local "A" (LNew ["N2"]), Comm "A" "B" (Comp "pair" [Atom "N1", Atom "N2"]), Local "B" (LNew ["N3"]), Comm "B" "A" (Comp "pair" [Atom "N2", Atom "N3"]) ]
  -- let actions = [ Local "A" (LNew ["N1", "N2"]), Comm "A" "B" (Comp "pair" [Atom "N1", Atom "N2"]), Local "B" (LNew ["N3"])]
  let actions = [ Local "A" (LNew ["N1", "N2"]), Comm "A" "B" (Comp "pair" [Comp "pair" [Atom "N1", Atom "N2"], Comp "h" [Atom "N1", Atom "N2"]])]

  let transactions = splitActionsForNewSession actions 
  putStrLn $ show transactions
  let st1:[] = V.execMState (V.addPublicFunction "h") V.initialState
  let (checkedTransactions, state):[] = V.runMState (checkTransactions transactions) st1
  putStrLn $ show state
  putStrLn $ show checkedTransactions
  putStrLn "\nGENERATED CODE:\n"
  putStrLn $ generateTransactions 1 checkedTransactions


