module Generator where

import Types (Process (NReceive, NSend, NTry, NIf, NNew, NChoice, NRead, NWrite, NRelease, NNil), CellDef, Def, Mode (MStar, MDiamond))
import qualified State as S
import qualified Helper as H
import qualified Data.List as List

generate :: S.Header -> [CellDef] -> [Process] -> Int -> String
-- generate the whole nn file
generate h cells transactions bound = do
  let sStr = generateSigmas h
  let cStr = generateCells cells
  let tStr = generateTransactions 1 transactions
  let bStr = "Bound: " ++ (show bound) ++ "\n"
  sStr ++ cStr ++ tStr ++ bStr

generateSigmas :: S.Header -> String
-- generate NN code for the sigma0 and sigma headers
generateSigmas h = do
  let (sig0, sig, sigP) = (S.s0 h, S.sPub h, S.sPriv h)
  let sig0Str = if sig0 == [] then "" else "\n  public " ++ (List.intercalate " " (map generateDef sig0))
  let sigStr = if sig == [] then "" else "\n  public " ++ (List.intercalate " " (map generateDef sig))
  let sigPStr = if sigP == [] then "" else "\n  private " ++ (List.intercalate " " (map generateDef sigP))
  (if sig0Str == "" then "" else "Sigma0:" ++ sig0Str) ++
    (if (sigStr ++ sigPStr) == "" then "" else "\nSigma:" ++ sigStr ++ sigPStr) ++ "\n\n"

generateDef :: Def -> String
-- generate NN representation of function definition
generateDef (label, arity) =
  label ++ "/" ++ (show arity)

generateCells :: [CellDef] -> String
-- generate NN code for cell section
generateCells cells = do
  let cellsStr = List.intercalate "\n" (map generateCell cells)
  "Cells:\n" ++ cellsStr ++ "\n\n"

generateCell :: CellDef -> String
-- generate NN code for cell definition
generateCell (cell, index, initVal) =
  cell ++ "[" ++ (H.msgToStr index) ++ "] := " ++ (H.msgToStr initVal) 

generateTransactions :: Int -> [Process] -> String
-- generate NN code from list of transactions
generateTransactions _ [] = ""
generateTransactions i (p:ps) = 
  "Transaction " ++ ("T" ++ show i) ++ ":\n" ++ (generateProcess p) ++ "\n\n" ++ (generateTransactions (i+1) ps)

generateMode :: Mode -> String
-- generate NN code for mode
generateMode MStar = "*"
generateMode MDiamond = "<>"

generateProcess :: Process -> String
-- generate NN code from processes
generateProcess (NNew xs r) =
  "new " ++ (List.intercalate "," xs) ++ ".\n" ++ (generateProcess r)
generateProcess (NSend rcp r) =
  "send " ++ (H.recipeToStr rcp) ++ ".\n" ++ (generateProcess r)
generateProcess (NReceive l r) =
  "receive " ++ l ++ ".\n" ++ (generateProcess r)
generateProcess (NTry l r ps1 ps2) =
  "try " ++ l ++ " = " ++ (H.recipeToStr r) ++ " in\n" ++ (generateProcess ps1) ++ "\ncatch " ++ (generateProcess ps2)
generateProcess (NIf f ps1 ps2) =
  "if " ++ (H.formulaToStr f) ++ " then\n" ++ (generateProcess ps1) ++ "\nelse " ++ (generateProcess ps2)
generateProcess (NChoice m x domain r) =
  (generateMode m) ++ " " ++ x ++ " in {" ++ (List.intercalate "," domain) ++ "}.\n" ++ (generateProcess r)
generateProcess (NRead label cell rcp r) =
  label ++ " := " ++ cell ++ "[" ++ (H.recipeToStr rcp) ++ "].\n" ++ (generateProcess r)
generateProcess (NWrite cell rcp1 rcp2 r) =
  cell ++ "[" ++ (H.recipeToStr rcp1) ++ "] := " ++ (H.recipeToStr rcp2) ++ ".\n" ++ (generateProcess r)
generateProcess (NRelease m f r) = 
  (generateMode m) ++ " " ++ (H.formulaToStr f) ++ ".\n" ++ (generateProcess r)
generateProcess NNil =
  "nil"
