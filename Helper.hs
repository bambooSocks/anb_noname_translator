module Helper where

import Types (
  Msg (Atom, Comp), Recipe (RAtom, RComp), Formula (BEq, BNot, BAnd, BTrue), Label, Agent, Action (Local, Comm, If),
  AgentAction (AReceive, ASend, AIf, ANew, APickDomain, ARead, AWrite, ARelease, ANil),
  NNProcess (PRead), NNCell)
import Data.List (intercalate)

extractLabel :: Maybe Label -> Label
extractLabel (Just l) = l
extractLabel (Nothing) = error "Could not extract label"

extractRecipe :: Maybe Recipe -> Recipe
extractRecipe (Just r) = r
extractRecipe (Nothing) = error "Could not extract recipe"

extractRecipes :: [Maybe Recipe] -> [Recipe]
extractRecipes mrs = map extractRecipe mrs

getActionAgent :: Action -> Agent
getActionAgent (Local agent aa) =
  if agent == (getAgentActionAgent aa) then
    agent
  else error "The agents for local actions does not match with the agent action's agent"
getActionAgent (Comm agent _ _) = agent
getActionAgent (If agent _ _ _) = agent

getAgentActionAgent :: AgentAction -> Agent
getAgentActionAgent (ASend a _) = a
getAgentActionAgent (AReceive a _ _) = a
getAgentActionAgent (AIf a f as1 as2) = do
  let agents1 = map getAgentActionAgent as1
  let agents2 = map getAgentActionAgent as2
  if (all (a==) agents1) && (all (a==) agents2) then
    a
  else error ("The agents for 'if " ++ (formulaToStr f) ++ " then' do not match")
getAgentActionAgent (ANew a _) = a
getAgentActionAgent (APickDomain a _ _) = a
getAgentActionAgent (ARead a _ _ _) = a
getAgentActionAgent (AWrite a _ _ _) = a
getAgentActionAgent (ARelease a _ _) = a
getAgentActionAgent (ANil) = ""

msgToStr :: Msg -> String
msgToStr (Atom x) = x
msgToStr (Comp id args) = do
  let argStr = intercalate "," (map msgToStr args)
  id ++ "(" ++ argStr ++ ")"

recipeToStr :: Recipe -> String
recipeToStr (RAtom x) = x
recipeToStr (RComp id args) = do
  let argStr = intercalate "," (map recipeToStr args)
  id ++ "(" ++ argStr ++ ")"

recipeToMsg :: Recipe -> Msg
recipeToMsg (RAtom x) = Atom x
recipeToMsg (RComp id args) = do
  let mArgs = map recipeToMsg args
  Comp id mArgs

formulaToStr :: Formula -> String
formulaToStr (BAnd f1 f2) = do
  "(" ++ (formulaToStr f1) ++ " && " ++ (formulaToStr f2) ++ ")"
formulaToStr (BNot f) = do
  "(not " ++ (formulaToStr f) ++ ")"
formulaToStr (BEq r1 r2) = do
  "(" ++ (recipeToStr r1) ++ " = " ++ (recipeToStr r2) ++ ")"
ormulaToStr (BTrue) = do
  "true"

unableErrorMsg :: Msg -> Agent -> String
unableErrorMsg msg agent = 
  "Unable to deduce a recipe for " ++ (msgToStr msg) ++ " from agent " ++ agent ++ "'s frame"

memCellPrefix :: String
memCellPrefix = "mem_"

getCellNameFromLabel :: Label -> NNCell
getCellNameFromLabel label =
  (memCellPrefix ++ label, Atom "S", Atom "null")

getReadProcessFromLabels :: [Label] -> [NNProcess]
getReadProcessFromLabels ls =
  map (\l -> PRead l (memCellPrefix ++ l) (Atom "S")) ls