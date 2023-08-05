module Helper where

import Types (
  Msg (Atom, Comp), Recipe (RPub, RLabel, RComp), Formula (BEq, BNot, BAnd, BOr, BTrue), Label, Agent, 
  NNProcess (NNRead), NNCell)
import ParserModel (Action (Local, Comm, End), PProcess (PIf))
import qualified Data.List as List

extractLabel :: Maybe Label -> Label
extractLabel (Just l) = l
extractLabel (Nothing) = error "Could not extract label"

extractRecipe :: Maybe Recipe -> Recipe
extractRecipe (Just r) = r
extractRecipe (Nothing) = error "Could not extract recipe"

extractRecipes :: [Maybe Recipe] -> [Recipe]
extractRecipes mrs = map extractRecipe mrs

getActionAgents :: Action -> [Agent]
-- get all agents related to an action
getActionAgents (Local agent (PIf _ rest1 rest2) _) = List.nub ([agent] ++ (getActionAgents rest1) ++ (getActionAgents rest2))
getActionAgents (Local agent _ rest) = List.nub ([agent] ++ (getActionAgents rest))
getActionAgents (Comm agent1 agent2 _ rest) = List.nub ([agent1, agent2] ++ (getActionAgents rest))
getActionAgents End = []

msgToStr :: Msg -> String
msgToStr (Atom x) = x
msgToStr (Comp id args) = do
  let argStr = List.intercalate "," (map msgToStr args)
  id ++ "(" ++ argStr ++ ")"

-- recipeToStr :: Recipe -> String
-- recipeToStr (RAtom x) = x
-- recipeToStr (RComp id args) = do
--   let argStr = List.intercalate "," (map recipeToStr args)
--   id ++ "(" ++ argStr ++ ")"

-- recipeToMsg :: Recipe -> Msg
-- recipeToMsg (RAtom x) = Atom x
-- recipeToMsg (RComp id args) = do
--   let mArgs = map recipeToMsg args
--   Comp id mArgs

formulaToStr :: Formula Msg -> String
formulaToStr (BAnd f1 f2) = do
  "(" ++ (formulaToStr f1) ++ " and " ++ (formulaToStr f2) ++ ")"
formulaToStr (BOr f1 f2) = do
  "(" ++ (formulaToStr f1) ++ " or " ++ (formulaToStr f2) ++ ")"
formulaToStr (BNot f) = do
  "(not " ++ (formulaToStr f) ++ ")"
formulaToStr (BEq m1 m2) = do
  "(" ++ (msgToStr m1) ++ " = " ++ (msgToStr m2) ++ ")"
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
  map (\l -> NNRead l (memCellPrefix ++ l) (Atom "S")) ls