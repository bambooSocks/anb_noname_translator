module Helper where

import Types (
  Msg (Atom, Comp), Recipe (RPub, RLabel, RComp), Formula (BEq, BNot, BAnd, BOr, BTrue), 
  Agent, Action (Local, Comm, End), PProcess (PIf))
import qualified Data.List as List

getActors :: Action -> [Agent]
-- get all agents related to an action chain
getActors (Local agent (PIf _ rest1 rest2) _) = List.nub ([agent] ++ (getActors rest1) ++ (getActors rest2))
getActors (Local agent _ rest) = List.nub ([agent] ++ (getActors rest))
getActors (Comm agent1 agent2 _ rest) = List.nub ([agent1, agent2] ++ (getActors rest))
getActors End = []

msgToStr :: Msg -> String
-- converts a message into a string
msgToStr (Atom x) = x
msgToStr (Comp id args) = do
  let argStr = List.intercalate "," (map msgToStr args)
  id ++ "(" ++ argStr ++ ")"

recipeToStr :: Recipe -> String
-- converts a recipe into a string
recipeToStr (RPub x) = x
recipeToStr (RLabel x) = x
recipeToStr (RComp id args) = do
  let argStr = List.intercalate "," (map recipeToStr args)
  id ++ "(" ++ argStr ++ ")"

formulaToStr :: Formula Recipe -> String
-- converts a recipe formula into a string
formulaToStr (BAnd f1 f2) = do
  "(" ++ (formulaToStr f1) ++ " and " ++ (formulaToStr f2) ++ ")"
formulaToStr (BOr f1 f2) = do
  "(" ++ (formulaToStr f1) ++ " or " ++ (formulaToStr f2) ++ ")"
formulaToStr (BNot f) = do
  "(not " ++ (formulaToStr f) ++ ")"
formulaToStr (BEq m1 m2) = do
  "(" ++ (recipeToStr m1) ++ " = " ++ (recipeToStr m2) ++ ")"
formulaToStr (BTrue) = do
  "true"

msgFormulaToStr :: Formula Msg -> String
-- converts a message formula into a string
msgFormulaToStr (BAnd f1 f2) = do
  "(" ++ (msgFormulaToStr f1) ++ " and " ++ (msgFormulaToStr f2) ++ ")"
msgFormulaToStr (BOr f1 f2) = do
  "(" ++ (msgFormulaToStr f1) ++ " or " ++ (msgFormulaToStr f2) ++ ")"
msgFormulaToStr (BNot f) = do
  "(not " ++ (msgFormulaToStr f) ++ ")"
msgFormulaToStr (BEq m1 m2) = do
  "(" ++ (msgToStr m1) ++ " = " ++ (msgToStr m2) ++ ")"
msgFormulaToStr (BTrue) = do
  "true"
