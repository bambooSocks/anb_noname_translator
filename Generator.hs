module Generator where

import Types (
  Msg (Atom, Comp), Marking(ToDo),
  Action (Local, Comm, If),
  Agent, Label, Check(CTry, CIf),
  AgentAction (AReceive, ASend, AIf, ANew, APickDomain, ARead, AWrite, ARelease, ANil),
  NNProcess (PReceive, PSend, PTry, PIf, PNew, PPickDomain, PRead, PWrite, PRelease, PNil), 
  NNTransaction, NNCell, NNDef
  Formula(BNot, BAnd, BEq, BTrue),
  Recipe(RAtom, RComp),
  Mode(MStar, MDiamond))
import qualified State as S
import qualified Helper as H
import qualified Verification as V
import Data.Foldable (for_)
import Data.Char (isUpper)
import qualified Data.List as List
import qualified Data.Set as Set

-- splitActionsNewSession :: [Action] -> [[AgentAction]]
-- -- split action into transactions with the new session ID action
-- splitActionsNewSession actions@(act:_) = do
--   let agent = H.getActionAgent act
--   if checkActionOrder actions agent then do
--     splitActions actions [[ANew agent ["S"]]] 1 -- TODO: this has to be consolidated with the first new
--   else
--     error "Actions do not have a consistent agent assignment"

-- checkActionOrder :: [Action] -> Agent -> Bool -- TODO: check whether the agents are defined or not
-- -- checks for the illegal action order
-- checkActionOrder [] _ = 
--   True
-- checkActionOrder ((Local agent _):as) lastAgent = do
--   (agent == lastAgent) && (checkActionOrder as agent)
-- checkActionOrder ((Comm agent1 agent2 _):as) lastAgent = do
--   (agent1 == lastAgent) && (checkActionOrder as agent2)
-- checkActionOrder ((If agent _ _ _):as) lastAgent = do
--   if as /= [] then
--     False
--   else do
--     (agent == lastAgent)

-- splitActions :: [Action] -> [[AgentAction]] -> Int -> [[AgentAction]]
-- -- split actions into corresponding transactions
-- splitActions [] (t:ts) index = do
--   let tts = ((t ++ [ANil]):ts)
--   reverse tts
-- splitActions ((Local agent aa):as) (t:ts) index = do
--   let newT = t ++ [aa]
--   splitActions as (newT:ts) index
-- splitActions ((Comm agent1 agent2 m):as) (t:ts) index = do
--   let newT = t ++ [ASend agent1 m, ANil]
--   let newTS = [AReceive agent2 m ("T" ++ (show (index + 1)))]:newT:ts
--   splitActions as newTS (index + 1)
-- splitActions ((If agent formula as1 as2):as) (t:ts) index = do
--   let ts1:_ = splitActions as1 [[]] 0
--   let ts2:_ = splitActions as2 [[]] 0
--   let newTS = (t ++ [AIf agent formula ts1 ts2]):ts
--   reverse newTS

-- convertCheck :: Check -> [NNProcess] -> NNProcess
-- -- convert checks into processes
-- convertCheck (CTry c) acc =
--   PTry c acc
-- convertCheck (CIf (l, r)) acc = do
--   PIf (BEq (RAtom l) r) acc [PNil]

-- convert :: [[AgentAction]] -> S.MState [NNTransaction]
-- -- convert split actions into proper transactions
-- convert aas = do
--   naas <- insertAgentPickers aas
--   ts <- convertTransactions naas
--   state <- S.get
--   pubLabels <- S.getPublicLabels
--   let missingLabels = getMissingLabels ts pubLabels
--   let tts = map (insertDependencies missingLabels) ts
--   S.put state { S.cellLabels = missingLabels ++ (S.cellLabels state) }
--   return tts

-- insertDependencies :: [Label] -> NNTransaction -> NNTransaction
-- -- insert all dependencies read and write processes into a transaction
-- insertDependencies [] t = t
-- insertDependencies (l:ls) t = do
--   let nt = insertDependency l t
--   insertDependencies ls nt

-- insertWrite :: Label -> NNTransaction -> NNTransaction -- TODO: this has to be done at the end not after the receiving action
-- -- insert memory write process after the process that introduces the variable
-- insertWrite _ [] =
--   []
-- insertWrite label ts@(p@(PIf f ps1 ps2):ps) = do
--   let wps1 = insertWrite label ps1
--   let wps2 = insertWrite label ps2
--   (PIf f wps1 wps2):(insertWrite label ps)
-- insertWrite label ts@(p@(PTry c@(l, _) pps):ps) = do
--   if (label == l) then do
--     let writeP = (PWrite (H.memCellPrefix ++ label) (Atom "S") (Atom label))
--     (PTry c (writeP:pps)):ps
--   else
--     (PTry c (insertWrite label pps)):(insertWrite label ps)
-- insertWrite label ts@(p:ps) = do
--   let (pcl, _) = getProcessLabels p (Set.empty, Set.empty)
--   if (elem label pcl) then
--     [p, (PWrite (H.memCellPrefix ++ label) (Atom "S") (Atom label))] ++ ps
--   else
--     p:(insertWrite label ps)

-- insertDependency :: Label -> NNTransaction -> NNTransaction
-- -- insert memory writing or reading to the right location in transaction
-- insertDependency _ [] = do
--   []
-- insertDependency label ts = do
--   let (cl, rl) = getTransactionLabels (Set.empty, Set.empty) ts
--   let ts1 = if (elem label cl) then insertWrite label ts else ts
--   if (elem label (rl Set.\\ cl)) then 
--     (PRead label (H.memCellPrefix ++ label) (Atom "S")):ts1 
--   else 
--     ts1

-- convertTransactions :: [[AgentAction]] -> S.MState [NNTransaction]
-- -- convert split agent actions into transactions
-- convertTransactions [] = do
--   return []
-- convertTransactions (aa:aas) = do
--   t <- convertTransaction aa
--   ts <- convertTransactions aas
--   return (t:ts)

-- updateRecipesForFormula :: Agent -> Formula -> S.MState Formula
-- -- check and update whether some of the 
-- updateRecipesForFormula agent f@(BEq r1 r2) = do
--   newR1 <- V.tryGetRecipe agent (H.recipeToMsg r1)
--   newR2 <- V.tryGetRecipe agent (H.recipeToMsg r2)
--   case (newR1, newR2) of
--     (Just nr1, Just nr2) -> return (BEq nr1 nr2)
--     _ -> error ("Cannot update the formula (" ++ (H.formulaToStr f) ++ ") with recipes")
-- updateRecipesForFormula agent (BAnd f1 f2) = do
--   uf1 <- updateRecipesForFormula agent f1
--   uf2 <- updateRecipesForFormula agent f2
--   return $ BAnd uf1 uf2
-- updateRecipesForFormula agent (BNot f) = do
--   uf <- updateRecipesForFormula agent f
--   return $ BNot uf
-- updateRecipesForFormula _ BTrue = do
--   return BTrue

-- convertTransaction :: [AgentAction] -> S.MState NNTransaction
-- -- convert agent action list to transaction
-- convertTransaction [] = do
--   return []
-- -- special case for receiving process, when the following processes have to be nested
-- convertTransaction ((AReceive agent msg tLabel):as) = do
--   (newLabel, checks) <- V.receive agent msg
--   ts <- convertTransaction as
--   let nestedChecks = foldr (\ c acc -> [convertCheck c acc]) ts checks 
--   return [
--     PReceive newLabel, 
--     PReceive "S",
--     PRead "T" "sessionTxn" (Atom "S"),
--     PIf (BEq (RAtom "T") (RAtom tLabel)) nestedChecks [PNil] ]
-- -- if action converter
-- convertTransaction ((AIf agent formula as1 as2):as) = do
--   newFormula <- updateRecipesForFormula agent formula
--   ts <- convertTransaction as
--   ts1 <- convertTransaction as1
--   ts2 <- convertTransaction as2
--   return $ [PIf newFormula ts1 ts2] ++ ts
-- -- default case for other actions
-- convertTransaction (a:as) = do
--   t <- convertAgentAction a
--   ts <- convertTransaction as
--   return $ t ++ ts

-- convertAgentAction :: AgentAction -> S.MState [NNProcess]
-- -- convert agent actions into processes
-- -- new nonce action converter
-- convertAgentAction (ANew agent xs) = do
--   for_ xs (\x -> V.registerFresh agent (Atom x) ToDo)
--   return [PNew xs]
-- -- send action converter
-- convertAgentAction (ASend agent msg) = do
--   result <- V.canDeduceFromFrame agent msg
--   if result then
--     return [PSend msg, PSend (Atom "S")]
--   else
--     error (H.unableErrorMsg msg agent)
-- -- domain pick action converter
-- convertAgentAction (APickDomain agent x domain) = do
--   let atoms = map (\l -> Atom l) domain
--   result <- V.canDeduceManyFromFrame agent atoms
--   if result then do
--     label <- V.registerFresh agent (Atom x) ToDo
--     return [PPickDomain x domain]
--   else
--     error "Unable to deduce a recipe for domain pick process from agent's frame"
-- -- write action converter
-- convertAgentAction (AWrite agent c m1 m2) = do
--   res1 <- V.canDeduceFromFrame agent m1
--   res2 <- V.canDeduceFromFrame agent m2
--   if res1 && res2 then
--     return [PWrite c m1 m2]
--   else
--     error "Unable to deduce a recipe for a write process from agent's frame"
-- -- read action converter
-- convertAgentAction (ARead agent l c m) = do
--   return [PRead l c m]
-- -- release action converter
-- convertAgentAction (ARelease agent mode formula) = do
--   return [PRelease mode formula]
-- -- nil action converter
-- convertAgentAction (ANil) = do
--   return [PNil]
-- -- default converts to nil
-- convertAgentAction a = do
--   return [PNil]

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

-- getMissingLabels :: [NNTransaction] -> [Label] -> [Label]
-- getMissingLabels ts pubLabels = do
--   let tLabels = map (getTransactionLabels (Set.empty, Set.empty)) ts
--   concatMap (\(cl, rl) -> Set.toList ((rl Set.\\ cl) Set.\\ (Set.fromList pubLabels))) tLabels

-- getTransactionLabels :: (Set.Set Label, Set.Set Label) -> NNTransaction -> (Set.Set Label, Set.Set Label)
-- -- returns a tuple of create Labels and required Labels for a transaction
-- getTransactionLabels (cl, rl) [] = 
--   (cl, rl)
-- getTransactionLabels (cl, rl) (p:ps) = do
--   let (pcl, prl) = getProcessLabels p (cl, rl)
--   getTransactionLabels (pcl, prl) ps

-- getProcessLabels :: NNProcess -> (Set.Set Label, Set.Set Label) -> (Set.Set Label, Set.Set Label)
-- -- returns a tuple of created Labels and required Labels for a process
-- getProcessLabels (PReceive l) (cl, rl) = 
--   (Set.insert l cl, rl)
-- getProcessLabels (PTry (l, r) t) (cl, rl) = do
--   let (ncl, nrl) = getTransactionLabels (cl, rl) t
--   (Set.insert l ncl, Set.union nrl (Set.fromList (getRequiredLabelsFromRecipe r)))
-- getProcessLabels (PIf f t1 t2) (cl, rl) =do
--   let (cl1, rl1) = getTransactionLabels (cl, rl) t1
--   let (cl2, rl2) = getTransactionLabels (cl1, rl1) t2
--   let nrl = Set.fromList (getRequiredLabelsFromFormula f)
--   (cl2, Set.union nrl rl2)
-- getProcessLabels (PNew ls) (cl, rl) =
--   (Set.union cl (Set.fromList ls), rl)
-- getProcessLabels (PSend m) (cl, rl) =
--   (cl, (Set.union rl (Set.fromList (getRequiredLabelsFromMessage m))))
-- getProcessLabels (PPickDomain x domain) (cl, rl) = 
--   (Set.insert x cl, Set.union rl (Set.fromList domain))
-- getProcessLabels (PRead l _ m) (cl, rl) =
--   (Set.insert l cl, Set.union rl (Set.fromList (getRequiredLabelsFromMessage m)))
-- getProcessLabels (PWrite _ m1 m2) (cl, rl) =
--   (cl, Set.union rl (Set.fromList((getRequiredLabelsFromMessage m1) ++ (getRequiredLabelsFromMessage m2))))
-- getProcessLabels (PRelease _ f) (cl, rl) =
--   (cl, Set.union rl (Set.fromList (getRequiredLabelsFromFormula f)))
-- getProcessLabels (PNil) (cl, rl) =
--   (cl, rl)

-- getRequiredLabelsFromRecipe :: Recipe -> [Label]
-- getRequiredLabelsFromRecipe (RAtom l) = [l]
-- getRequiredLabelsFromRecipe (RComp _ args) =
--   concatMap getRequiredLabelsFromRecipe args

-- getRequiredLabelsFromMessage :: Msg -> [Label]
-- getRequiredLabelsFromMessage (Atom l) = [l]
-- getRequiredLabelsFromMessage (Comp _ args) =
--   concatMap getRequiredLabelsFromMessage args

-- getRequiredLabelsFromFormula :: Formula -> [Label]
-- getRequiredLabelsFromFormula (BEq r1 r2) =
--   ((getRequiredLabelsFromRecipe r1) ++ (getRequiredLabelsFromRecipe r2))
-- getRequiredLabelsFromFormula _ = []


-- NN code generation

generate :: [NNDef] -> [NNDef] -> [NNDef] -> [NNCell] -> [NNTransaction] -> Int -> String
generate sig0 sig sigP cells transactions bound = do
  let sStr = generateSigmas sig0 sig sigP
  let cStr = generateCells cells
  let tStr = generateTransactions 1 transactions
  let bStr = "Bound: " ++ (show bound) ++ "\n"
  sStr ++ cStr ++ tStr ++ bStr

generateSigmas :: [NNDef] -> [NNDef] -> [NNDef] -> String
-- generate NN code for the sigma0 and sigma headers
generateSigmas sig0 sig sigP = do
  let sig0Str = if sig0 == [] then "" else "\n  public " ++ (List.intercalate " " (map generateDef sig0))
  let sigStr = if sig == [] then "" else "\n  public " ++ (List.intercalate " " (map generateDef sig))
  let sigPStr = if sigP == [] then "" else "\n  private " ++ (List.intercalate " " (map generateDef sigP))
  
  (if sig0Str == "" then "" else "Sigma0:" ++ sig0Str) ++
    (if (sigStr ++ sigPStr) == "" then "" else "\nSigma:" ++ sigStr ++ sigPStr) ++ "\n\n"

generateDef :: NNDef -> String
-- generate NN representation of function definition
generateDef (label, arity) =
  label ++ "/" ++ (show arity)

generateCells :: [NNCell] -> String
-- generate NN code for cell section
generateCells cells = do
  let cellsStr = List.intercalate "\n" (map generateCell cells)
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
  "new " ++ (List.intercalate "," xs) ++ ".\n"
generateProcess (PSend msg) =
  "send " ++ (H.msgToStr msg) ++ ".\n"
generateProcess (PReceive l) =
  "receive " ++ l ++ ".\n"
generateProcess (PTry (l,r) ps) = do
  let subProcesses = concatMap generateProcess ps
  "try " ++ l ++ " = " ++ (H.recipeToStr r) ++ " in\n" ++ subProcesses ++ "\ncatch nil"
generateProcess (PIf f ps1 ps2) = do
  let ifProcesses = concatMap generateProcess ps1
  let elseProcesses = concatMap generateProcess ps2
  "if " ++ (H.formulaToStr f) ++ " then\n" ++ ifProcesses ++ "\nelse " ++ elseProcesses
generateProcess (PPickDomain x domain) = do
  "* " ++ x ++ " in {" ++ (List.intercalate "," domain) ++ "}.\n"
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
  -- if statements
  -- let actions = [Local "A" (APickDomain "A" "x" ["a", "b"]), Comm "A" "B" (Atom "x"), Local "B" (ANew "A" ["TEST"]), If "B" (BEq (RAtom "x") (RAtom "a")) [Comm "B" "A" (Atom "ok")] [Comm "B" "A" (Atom "wrong")]]

  -- process actions
  let splitActs = splitActionsNewSession actions
  putStrLn "Split Actions:"
  putStrLn $ show splitActs
  -- load sigmas
  let st:[] = S.execMState 
                (S.addInitialState 
                  (length splitActs) 
                  [("ok", 0), ("wrong", 0)] 
                  [("h", 2)] 
                  []
                  ["A","B"]
                  ["a","b"]
                  ["i"]) S.initialState
  let initialCells = [("sessionTxn", Atom "S", Atom "T1")]

  putStrLn "\nState:"
  let (transactions, state):[] = S.runMState (convert splitActs) st
  putStrLn $ show state

  putStrLn "\nTransactions:"
  putStrLn $ show transactions

  putStrLn "\nGENERATED CODE:\n"
  let cells = map H.getCellNameFromLabel (S.cellLabels state)
  putStrLn $ generateSigmas (S.sigma0 state) (S.sigma state) (S.sigmaPriv state)
  putStrLn $ generateCells (initialCells ++ cells)
  putStrLn $ generateTransactions 1 transactions


