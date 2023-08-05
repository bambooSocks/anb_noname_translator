module Verification where

import Types (Msg(Atom, Comp), 
          Agent, Label, NNCell,
          Check(CTry, CIf), 
          Recipe(RAtom, RComp),
          Marking(ToDo, Done), Frame)
import Data.Foldable (find)
import qualified Data.Map as M
import qualified Helper as H
import qualified State as S


putFrameForAgent :: Agent -> Frame -> S.MState ()
-- helper to update a frame for a given agent
putFrameForAgent agent frame = do
  state <- S.get
  let afs = (S.frames state)
  S.put state { S.frames = M.insert agent frame afs }

isInAgentsFrame :: Agent -> Msg -> S.MState Bool
-- checks whether the message is in agent's frame
isInAgentsFrame agent msg = do
  frame <- getFrameForAgent agent
  let msgs = M.map fst frame
  return (any (msg ==) msgs)

initAgentsFrame :: Agent -> [Msg] -> S.MState ()
-- initialize agents frame with initial knowledge
initAgentsFrame agent [] = do
  return ()
initAgentsFrame agent (msg:msgs) = do
  registerFresh agent msg ToDo
  initAgentsFrame agent msgs

receive :: Agent -> Msg -> S.MState (Label,[Check])
receive agent msg@(Atom x) = do
  cond <- canDeduceFromFrame agent msg
  label <- freshLabel
  register agent msg ToDo label
  checks <- decomposeToDo agent
  if cond then
    return (label, [CIf (label, RAtom x)] ++ checks)
  else do
    return (label, checks)
receive agent msg = do
  label <- freshLabel
  register agent msg ToDo label
  checks <- decomposeToDo agent
  return (label, checks)


-- TODO: clean up main

main :: IO ()
main = do
  -- composition check example
  let x = Comp "pair" [Atom "sk", Atom "N"]
  let xx = Comp "h" [Atom "sk", Atom "N"]

  let initKnowledgeA = [Atom "A", Comp "pk" [Atom "A"], Comp "inv" [Comp "pk" [Atom "A"]]]
  let stt0 = initAgentsFrame "A" initKnowledgeA
  let stt1 = receive "A" x
  let st1:[] = S.execMState stt1 S.initialState
  let canDeduce = S.evalMState (canDeduceFromFrame "A" (Comp "scrypt" [Atom "sk", Atom "secret"])) st1
  let canDeduce2 = S.evalMState (canDeduceFromFrame "A" (Atom "secret")) st1
  let stt2 = receive "A" xx
  let st2:[] = S.execMState stt2 st1
  let canDeduce3 = S.evalMState (canDeduceFromFrame "A" (Atom "secret")) st2
  putStrLn $ show st1
  putStrLn $ show st2
  putStrLn $ show canDeduce
  putStrLn $ show canDeduce2
  putStrLn $ show canDeduce3