module Generator where

import Types (
  Msg (Atom, Comp), 
  Agent, 
  UAction (UNew, UPickDomain), 
  NNProcess (PNew, PPickDomain, PReceive, PSend, PTry), 
  NNTransaction, 
  Cond(NotDefined))

import qualified Verification as V
import Data.Foldable (for_)

data Action
  = Unary Agent UAction -- would operation be better?
  | Binary Agent Agent Msg
  deriving (Show)

convertUActionToProcess :: Agent -> UAction -> NNProcess
-- convert unary action into a process
convertUActionToProcess agent (UNew ns) = PNew agent ns
convertUActionToProcess agent (UPickDomain x d) = PPickDomain agent x d

splitActions :: [Action] -> [NNTransaction] -> [NNTransaction]
-- split actions into corresponding transactions
splitActions [] ts =
  reverse ts
splitActions ((Unary agent ua):as) (t:ts) = do
  let p = convertUActionToProcess agent ua
  let newT = t ++ [p]
  splitActions as (newT:ts)
splitActions ((Binary agent1 agent2 m):as) (t:ts) = do
  let newT = t ++ [PSend agent1 m]
  let newTS = [PReceive agent2 m]:newT:ts
  splitActions as newTS

-- Q: can this be simplified with fold??? ... not sure how to do it when using state
verifyTransactions :: [NNTransaction] -> V.MState [NNTransaction]
-- verify that multiple transactions can be run
verifyTransactions [] = do
  return []
verifyTransactions (t:ts) = do
  tt <- verifyTransaction t
  tts <- verifyTransactions ts
  return (tt:tts)

verifyTransaction :: NNTransaction -> V.MState NNTransaction
-- verify that singe transaction can be run
verifyTransaction [] = do
  return []
verifyTransaction (p:ps) = do
  t <- verifyProcess p
  ts <- verifyTransaction ps
  return (t ++ ts)
  
verifyProcess :: NNProcess -> V.MState NNTransaction
-- verify that single process can be run
verifyProcess p@(PNew agent xs) = do
  for_ xs (\x -> V.registerFresh agent (Atom x) V.ToDo)
  return [p]
verifyProcess p@(PReceive agent msg) = do
  V.receive agent msg
  -- TODO: add try checking for specific cases
  return [p, PTry agent NotDefined []]
verifyProcess p@(PSend agent msg) = do
  result <- V.canDeduceFromFrame agent msg
  if result then
    return [p]
  else
    -- TODO: error
    return []

main :: IO ()
main = do
  let actions = [ Unary "A" (UNew ["N1"]), Unary "A" (UNew ["N2"]), Binary "A" "B" (Comp "pair" [Atom "N1", Atom "N2"]), Unary "B" (UNew ["N3"]), Binary "B" "A" (Comp "pair" [Atom "N2", Atom "N3"]) ]
  let transactions = splitActions actions [[]]  
  putStrLn $ show transactions
  let checkedTransactions = V.evalMState (verifyTransactions transactions) V.initialState
  putStrLn $ show checkedTransactions


