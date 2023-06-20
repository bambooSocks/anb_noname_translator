module Generator where

import Types (
  Msg (Atom, Comp), 
  Agent, Label, Check(CTry, CIf), 
  -- LocalAction (LNew, LPickDomain, LRead, LWrite, LRelease), 
  AgentAction (AReceive, ASend, AIf, ANew, APickDomain, ARead, AWrite, ARelease, ANil),
  NNProcess (PReceive, PSend, PTry, PIf, PCheckIf, PNew, PPickDomain, PRead, PWrite, PRelease, PNil), 
  NNTransaction, NNCell,
  Formula(BNot, BAnd, BEq, BTrue),
  Recipe(RAtom, RComp),
  Mode(MStar, MDiamond),
  unableErrorMsg, msgToStr, recipeToStr, formulaToStr)

import qualified Verification as V
import Data.Foldable (for_)
import Data.List (intercalate)

data Action
  = Local Agent AgentAction
  | Comm Agent Agent Msg
  deriving (Show)

splitActionsNewSession :: [Action] -> [[AgentAction]]
splitActionsNewSession actions =
  splitActions actions [[ANew "" ["S"]]] 1

splitActions :: [Action] -> [[AgentAction]] -> Int -> [[AgentAction]]
-- split actions into corresponding transactions
splitActions [] (t:ts) index = do
  let tts = ((t ++ [ANil]):ts)
  reverse tts
splitActions ((Local agent aa):as) (t:ts) index = do
  let newT = t ++ [aa]
  splitActions as (newT:ts) index
splitActions ((Comm agent1 agent2 m):as) (t:ts) index = do
  let newT = t ++ [ASend agent1 m, ANil]
  let newTS = [AReceive agent2 m ("T" ++ (show (index + 1)))]:newT:ts
  splitActions as newTS (index + 1)

convertCheck :: Check -> [NNProcess] -> NNProcess
convertCheck (CTry c) acc =
  PTry c acc
convertCheck (CIf c) acc = do
  PCheckIf c acc

convert :: [[AgentAction]] -> V.MState ([NNTransaction], [NNCell])
-- checks and converts split agent actions into transactions
convert [] = do
  return ([],[])
convert (t:ts) = do
  (tt, cs1) <- convertTransaction t
  (tts, cs2) <- convert ts
  return ((tt:tts), (cs1 ++ cs2))

convertTransaction :: [AgentAction] -> V.MState (NNTransaction, [NNCell])
-- check that singe transaction can be run
convertTransaction [] = do
  return ([],[])
-- special case for receiving process, when the following processes have to be nested
convertTransaction ((AReceive agent msg tLabel):as) = do
  (newLabel, checks) <- V.receive agent msg
  (ts, cs) <- convertTransaction as
  let nestedChecks = foldr (\ c acc -> [convertCheck c acc]) ts checks 
  let newCell = ("rcv" ++ newLabel)
  return ([
    PReceive newLabel, 
    PReceive "S", 
    PRead "T" "sessionTxn" (Atom "S"),
    PIf (BEq (RAtom "T") (RAtom tLabel)) ([PWrite newCell (Atom "S") (Atom newLabel)] ++ nestedChecks) ],
    [(newCell, Atom "S", Atom "0")] ++ cs) -- TODO: find a better default value or return the extra sigma (T1, ..., 0)
-- -- if action converter --TODO: finish me
-- convertTransaction ((AIf agent formula a1 a2):as) = do
  
-- default case for other actions
convertTransaction (a:as) = do
  t <- convertAgentAction a
  (ts, cs) <- convertTransaction as
  return ((t ++ ts), cs)

convertAgentAction :: AgentAction -> V.MState [NNProcess]
-- new nonce action converter
convertAgentAction (ANew agent xs) = do
  for_ xs (\x -> V.registerFresh agent (Atom x) V.ToDo)
  return [PNew xs]
-- send action converter
convertAgentAction (ASend agent msg) = do
  result <- V.canDeduceFromFrame agent msg
  if result then
    return [PSend msg, PSend (Atom "S")]
  else
    error (unableErrorMsg msg)
-- domain pick action converter
convertAgentAction (APickDomain agent x domain) = do
  result <- V.canDeduceManyFromFrame agent domain -- TODO: check also the sigmas for domain
  if result then
    return [PPickDomain x domain]
  else
    error "Unable to deduce a recipe for domain pick process from agent's frame"
-- write action converter
convertAgentAction (AWrite agent c m1 m2) = do
  res1 <- V.canDeduceFromFrame agent m1
  res2 <- V.canDeduceFromFrame agent m2
  if res1 && res2 then
    return [PWrite c m1 m2]
  else
    error "Unable to deduce a recipe for a write process from agent's frame"
-- read action converter
convertAgentAction (ARead agent l c m) = do
  return [PRead l c m]
-- release action converter
convertAgentAction (ARelease agent mode formula) = do
  return [PRelease mode formula]
-- nil action converter
convertAgentAction (ANil) = do
  return [PNil]
-- default converts to nil
convertAgentAction a = do
  return [PNil]


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
generateProcess (PNew xs) =
  "new(" ++ (intercalate "," xs) ++ ").\n"
generateProcess (PSend msg) =
  "send(" ++ (msgToStr msg) ++ ").\n"
generateProcess (PReceive l) =
  "receive(" ++ l ++ ").\n"
generateProcess (PTry (l,r) ps) = do
  let subProcesses = concatMap generateProcess ps
  "try " ++ l ++ "=" ++ (recipeToStr r) ++ " in\n" ++ subProcesses ++ "\ncatch nil"
generateProcess (PCheckIf (l,r) ps) = do
  let subProcesses = concatMap generateProcess ps
  "if " ++ l ++ "=" ++ (recipeToStr r) ++ " then\n" ++ subProcesses ++ "\nelse nil"
generateProcess (PIf f ps) = do
  let subProcesses = concatMap generateProcess ps
  "if " ++ (formulaToStr f) ++ " then\n" ++ subProcesses ++ "\nelse nil"
generateProcess (PPickDomain m ms) = do
  let domain = map msgToStr ms
  "* " ++ (msgToStr m) ++ "in {" ++ (intercalate "," domain) ++ "}.\n"
generateProcess (PRead label cell msg) =
  label ++ " := " ++ cell ++ "[" ++ (msgToStr msg) ++ "].\n"
generateProcess (PWrite cell msg1 msg2) =
  cell ++ "[" ++ (msgToStr msg1) ++ "] := " ++ (msgToStr msg2) ++ ".\n"
generateProcess (PRelease MStar f) = 
  "* " ++ (formulaToStr f) ++ ".\n"
generateProcess (PRelease MDiamond f) = 
  "<> " ++ (formulaToStr f) ++ ".\n"
generateProcess PNil =
  "nil"

main :: IO ()
main = do
  -- let actions = [ Local "A" (LNew ["N1"]), Local "A" (LNew ["N2"]), Comm "A" "B" (Comp "pair" [Atom "N1", Atom "N2"]), Local "B" (LNew ["N3"]), Comm "B" "A" (Comp "pair" [Atom "N2", Atom "N3"]) ]
  -- let actions = [ Local "A" (LNew ["N1", "N2"]), Comm "A" "B" (Comp "pair" [Atom "N1", Atom "N2"]), Local "B" (LNew ["N3"])]
  let actions = [ Local "A" (ANew "A" ["N1", "N2"]), Comm "A" "B" (Comp "pair" [Comp "pair" [Atom "N1", Atom "N2"], Comp "h" [Atom "N1", Atom "N2"]])]

  let splitActs = splitActionsNewSession actions 
  putStrLn $ show splitActs
  let st1:[] = V.execMState (V.addPublicFunction "h") V.initialState
  let initialCells = [("sessionTxn", Atom "S", Atom "T1")]
  let ((transactions, cells), state):[] = V.runMState (convert splitActs) st1
  putStrLn $ show state
  putStrLn $ show transactions
  putStrLn "\nGENERATED CODE:\n"
  putStrLn $ generateCells (initialCells ++ cells)
  putStrLn $ generateTransactions 1 transactions


