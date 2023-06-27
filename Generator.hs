module Generator where

import Types (
  Msg (Atom, Comp), Marking(ToDo),
  Action (Local, Comm, If),
  Agent, Label, Check(CTry, CIf),
  AgentAction (AReceive, ASend, AIf, ANew, APickDomain, ARead, AWrite, ARelease, ANil),
  NNProcess (PReceive, PSend, PTry, PIf, PNew, PPickDomain, PRead, PWrite, PRelease, PNil), 
  NNTransaction, NNCell,
  Formula(BNot, BAnd, BEq, BTrue),
  Recipe(RAtom, RComp),
  Mode(MStar, MDiamond))
import qualified State as S
import qualified Helper as H
import qualified Verification as V
import Data.Foldable (for_)
import Data.List (intercalate)
import qualified Data.Set as Set

splitActionsNewSession :: [Action] -> [[AgentAction]]
-- split action into transactions with the new session ID action
splitActionsNewSession actions@(act:_) = do
  let agent = H.getActionAgent act
  splitActions actions [[ANew agent ["S"]]] 1

splitActions :: [Action] -> [[AgentAction]] -> Int -> [[AgentAction]]
-- split actions into corresponding transactions
splitActions [] (t:ts) index = do
  let tts = ((t ++ [ANil]):ts)
  reverse tts
splitActions ((Local agent aa):as) (t:ts) index =
  if all (\a -> agent == (H.getAgentActionAgent a)) t then do
    let newT = t ++ [aa]
    splitActions as (newT:ts) index
  else
    error "Actions do not have a consistent agent"
splitActions ((Comm agent1 agent2 m):as) (t:ts) index =
  if all (\a -> agent1 == (H.getAgentActionAgent a)) t then do
    let newT = t ++ [ASend agent1 m, ANil]
    let newTS = [AReceive agent2 m ("T" ++ (show (index + 1)))]:newT:ts
    splitActions as newTS (index + 1)
  else
    error "Actions do not have a consistent agent"
splitActions ((If agent formula as1 as2):as) (t:ts) index =
  if as /= [] then
    error "If statement cannot be followed by another action"
  else do
    let ts1:_ = splitActions as1 [] 0
    let ts2:_ = splitActions as2 [] 0
    let newTS = ([AIf agent formula ts1 ts2] ++ t):ts
    splitActions [] newTS (index + 1)

convertCheck :: Check -> [NNProcess] -> NNProcess
-- convert checks into processes
convertCheck (CTry c) acc =
  PTry c acc
convertCheck (CIf (l, r)) acc = do
  PIf (BEq (RAtom l) r) acc [PNil]

convert :: [[AgentAction]] -> S.MState [NNTransaction]
-- convert split actions into proper transactions
convert aas = do
  state <- S.get
  pubLabels <- S.getPublicLabels
  ts <- convertTransactions aas
  let labels = map getTransactionLabels ts
  let (tts, ls) = unzip (map (updateTransaction pubLabels) (zip ts labels) )
  -- error ("Labels " ++ (show labels))
  S.put state { S.cellLabels = (concat ls) ++ (S.cellLabels state) }
  return tts

updateTransaction ::  [Label] -> (NNTransaction, (Set.Set Label, Set.Set Label)) -> (NNTransaction, [Label])
updateTransaction pubLabels (t, (cl, rl)) = do
  let missingLabels = Set.toList ((rl Set.\\ cl) Set.\\ (Set.fromList pubLabels))
  let newT = insertDependencies t missingLabels
  (newT, missingLabels)

insertDependencies :: NNTransaction -> [Label] -> NNTransaction
insertDependencies t [] = t
insertDependencies t (l:ls) = do
  let newT = insertDependency t l
  insertDependencies newT ls

insertDependency :: NNTransaction -> Label -> NNTransaction
insertDependency (p:ps) label = do
  let (cl, rl) = getProcessLabels p
  if (elem label cl) then
    [p, (PWrite ("rcv" ++ label) (Atom "S") (Atom label))] ++ ps
  else
    if (elem label rl) then
      [(PRead label ("rcv" ++ label) (Atom "S")), p] ++ ps
    else
      p:(insertDependency ps label)

convertTransactions :: [[AgentAction]] -> S.MState [NNTransaction]
-- convert split agent actions into transactions
convertTransactions [] = do
  return []
convertTransactions (aa:aas) = do
  t <- convertTransaction aa
  ts <- convertTransactions aas
  return (t:ts)

convertTransaction :: [AgentAction] -> S.MState NNTransaction
-- convert agent action list to transaction
convertTransaction [] = do
  return []
-- special case for receiving process, when the following processes have to be nested
convertTransaction ((AReceive agent msg tLabel):as) = do
  (newLabel, checks) <- V.receive agent msg
  ts <- convertTransaction as
  let nestedChecks = foldr (\ c acc -> [convertCheck c acc]) ts checks 
  -- let readActions = getReadProcessFromLabels ls -- TODO: add read actions if needed
  return [
    PReceive newLabel, 
    PReceive "S", 
    PRead "T" "sessionTxn" (Atom "S"), -- TODO: add the write if it is neede later [PWrite ("rcv" ++ newLabel) (Atom "S") (Atom newLabel)]
    PIf (BEq (RAtom "T") (RAtom tLabel)) nestedChecks [PNil] ]
-- if action converter --TODO: finish me (not tested)
convertTransaction ((AIf agent formula as1 as2):as) = do
  ts <- convertTransaction as
  ts1 <- convertTransaction as1
  ts2 <- convertTransaction as2
  return $ [PIf formula ts1 ts2] ++ ts
-- default case for other actions
convertTransaction (a:as) = do
  t <- convertAgentAction a
  ts <- convertTransaction as
  return $ t ++ ts

convertAgentAction :: AgentAction -> S.MState [NNProcess]
-- convert agent actions into processes
-- new nonce action converter
convertAgentAction (ANew agent xs) = do
  for_ xs (\x -> V.registerFresh agent (Atom x) ToDo)
  return [PNew xs]
-- send action converter
convertAgentAction (ASend agent msg) = do
  result <- V.canDeduceFromFrame agent msg
  if result then
    return [PSend msg, PSend (Atom "S")]
  else
    error (H.unableErrorMsg msg agent)
-- domain pick action converter
convertAgentAction (APickDomain agent x domain) = do
  let atoms = map (\l -> Atom l) domain
  result <- V.canDeduceManyFromFrame agent atoms
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

getTransactionLabels :: NNTransaction -> (Set.Set Label, Set.Set Label)
-- returns a tuple of create Labels and required Labels for a transaction
getTransactionLabels [] = 
  (Set.empty, Set.empty)
getTransactionLabels (p:ps) = do
  let (i, o) = getProcessLabels p
  let (ii, oo) = getTransactionLabels ps
  (Set.union i ii, Set.union o oo)

getProcessLabels :: NNProcess -> (Set.Set Label, Set.Set Label)
-- returns a tuple of created Labels and required Labels for a process
getProcessLabels (PReceive l) = 
  (Set.fromList [l], Set.empty)
getProcessLabels (PTry (l, r) _) =
  (Set.fromList [l], Set.fromList (getRequiredLabelsFromRecipe r))
getProcessLabels (PIf f t1 t2) = do
  let (cl1, rl1) = getTransactionLabels t1
  let (cl2, rl2) = getTransactionLabels t2
  let rl = Set.fromList (getRequiredLabelsFromFormula f)
  (Set.union cl1 cl2, Set.union (Set.union rl rl1) rl2)
getProcessLabels (PNew ls) =
  (Set.fromList ls, Set.empty)
getProcessLabels (PSend m) =
  (Set.empty, Set.fromList (getRequiredLabelsFromMessage m))
getProcessLabels (PPickDomain x domain) = 
  (Set.fromList [x], Set.fromList domain)
getProcessLabels (PRead l _ m) =
  (Set.fromList [l], Set.fromList (getRequiredLabelsFromMessage m))
getProcessLabels (PWrite _ m1 m2) =
  (Set.empty, Set.fromList((getRequiredLabelsFromMessage m1) ++ (getRequiredLabelsFromMessage m2)))
getProcessLabels (PRelease _ f) =
  (Set.empty, Set.fromList (getRequiredLabelsFromFormula f))
getProcessLabels (PNil) =
  (Set.empty, Set.empty)

getRequiredLabelsFromRecipe :: Recipe -> [Label]
getRequiredLabelsFromRecipe (RAtom l) = [l]
getRequiredLabelsFromRecipe (RComp _ args) =
  concatMap getRequiredLabelsFromRecipe args

getRequiredLabelsFromMessage :: Msg -> [Label]
getRequiredLabelsFromMessage (Atom l) = [l]
getRequiredLabelsFromMessage (Comp _ args) =
  concatMap getRequiredLabelsFromMessage args

getRequiredLabelsFromFormula :: Formula -> [Label]
getRequiredLabelsFromFormula (BEq r1 r2) =
  ((getRequiredLabelsFromRecipe r1) ++ (getRequiredLabelsFromRecipe r2))
getRequiredLabelsFromFormula _ = []


-- NN code generation

generateSigmas :: [(Label, Int)] -> [(Label, Int)] -> [(Label, Int)] -> String
-- generate NN code for the sigma0 and sigma headers
generateSigmas sig0 sig sigP = do
  let sig0Str = if sig0 == [] then "" else "\n  public " ++ (intercalate ", " (map generateDef sig0))
  let sigStr = if sig == [] then "" else "\n  public " ++ (intercalate ", " (map generateDef sig))
  let sigPStr = if sigP == [] then "" else "\n  private " ++ (intercalate ", " (map generateDef sigP))
  
  (if sig0Str == "" then "" else "Sigma0:" ++ sig0Str) ++
    (if (sigStr ++ sigPStr) == "" then "" else "\nSigma:" ++ sigStr ++ sigPStr) ++ "\n\n"

generateDef :: (Label, Int) -> String
-- generate NN representation of function definition
generateDef (label, arity) =
  label ++ "/" ++ (show arity)

generateCells :: [NNCell] -> String
-- generate NN code for cell section
generateCells cells = do
  let cellsStr = intercalate "\n" (map generateCell cells)
  "Cells:\n" ++ cellsStr ++ "\n"

generateCell :: NNCell -> String
-- generate NN code for cell definition
generateCell (cell, index, initVal) =
  cell ++ "[" ++ (H.msgToStr index) ++ "] := " ++ (H.msgToStr initVal) 

generateTransactions :: Int -> [NNTransaction] -> String
-- generate NN code from list of transactions
generateTransactions _ [] = 
  ""
generateTransactions i (t:ts) =
  (generateTransaction ("T" ++ show i) t) ++ (generateTransactions (i+1) ts)

generateTransaction :: String -> NNTransaction -> String
-- generate NN code from transaction
generateTransaction name ps = do
  let processes = concatMap generateProcess ps
  "Transaction " ++ name ++ ":\n" ++ processes ++ "\n\n"

generateProcess :: NNProcess -> String
-- generate NN code from processes
generateProcess (PNew xs) =
  "new " ++ (intercalate "," xs) ++ ".\n"
generateProcess (PSend msg) =
  "send " ++ (H.msgToStr msg) ++ ".\n"
generateProcess (PReceive l) =
  "receive " ++ l ++ ".\n"
generateProcess (PTry (l,r) ps) = do
  let subProcesses = concatMap generateProcess ps
  "try " ++ l ++ "=" ++ (H.recipeToStr r) ++ " in\n" ++ subProcesses ++ "\ncatch nil"
generateProcess (PIf f ps1 ps2) = do
  let ifProcesses = concatMap generateProcess ps1
  let elseProcesses = concatMap generateProcess ps2
  "if " ++ (H.formulaToStr f) ++ " then\n" ++ ifProcesses ++ "\nelse " ++ elseProcesses
generateProcess (PPickDomain x domain) = do
  "* " ++ x ++ "in {" ++ (intercalate "," domain) ++ "}.\n"
generateProcess (PRead label cell msg) =
  label ++ " := " ++ cell ++ "[" ++ (H.msgToStr msg) ++ "].\n"
generateProcess (PWrite cell msg1 msg2) =
  cell ++ "[" ++ (H.msgToStr msg1) ++ "] := " ++ (H.msgToStr msg2) ++ ".\n"
generateProcess (PRelease MStar f) = 
  "* " ++ (H.formulaToStr f) ++ ".\n"
generateProcess (PRelease MDiamond f) = 
  "<> " ++ (H.formulaToStr f) ++ ".\n"
generateProcess PNil =
  "nil"

-- main

main :: IO ()
main = do
  -- let actions = [ Local "A" (ANew "A" ["N1", "N2"]), Comm "A" "B" (Comp "pair" [Comp "pair" [Atom "N1", Atom "N2"], Comp "h" [Atom "N1", Atom "N2"]])]
  -- delayed check 1
  -- let actions = [Local "A" (ANew "A" ["N1", "N2"]), Comm "A" "B" (Comp "h" [Atom "N1", Atom "N2"]), Comm "B" "A" (Atom "ok"), Comm "A" "B" (Comp "pair" [Atom "N1", Atom "N2"]), Comm "B" "A" (Atom "ok")]
  -- delayed check 2
  let actions = [Local "A" (ANew "A" ["N1", "N2"]), Comm "A" "B" (Comp "pair" [Atom "N1", Comp "h" [Atom "N1", Atom "N2"]]), Comm "B" "A" (Atom "ok"), Comm "A" "B" (Atom "N2"), Comm "B" "A" (Atom "ok")]

  -- load header info
  let st:[] = S.execMState (S.addToSigma0 "ok" 0) S.initialState
  let initialCells = [("sessionTxn", Atom "S", Atom "T1")]
  let st1:[] = S.execMState (S.addPublicFunction "h" 2) st

  -- process actions
  let splitActs = splitActionsNewSession actions
  putStrLn "Split Actions:"
  putStrLn $ show splitActs
  -- add Transaction names to sigma0
  let st2:[] = S.execMState (S.addTransactionLabelsToSigma0 (length splitActs)) st1
  putStrLn "\nState:"
  let (transactions, state):[] = S.runMState (convert splitActs) st2
  putStrLn $ show state
  putStrLn "\nTransactions:"
  putStrLn $ show transactions
  putStrLn "\nGENERATED CODE:\n"
  let cells = map getCellNameFromLabel (S.cellLabels state)
  putStrLn $ generateSigmas (S.sigma0 state) (S.sigma state) (S.sigmaPriv state)
  putStrLn $ generateCells (initialCells ++ cells)
  putStrLn $ generateTransactions 1 transactions


