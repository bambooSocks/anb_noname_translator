module Main where

import System.Environment
import qualified State as S
import qualified Analysis as A
import qualified Generator as G
import qualified AnnB_Lexer as AL
import qualified AnnB_Parser as AP

main :: IO ()
main = do
  args <- getArgs
  case args of 
    [file] -> do
      x <- readFile file
      -- let initialKnowledge = [("A", [Atom "A", Atom "B"]), ("B", [Atom "A", Atom "B"])]
      -- let actions = Local "A" (PChoice MStar "X" ["a", "b"]) (Comm "A" "B" (Atom "X") (Local "B" (PIf (BEq (Atom "X") (Atom "a")) (Comm "B" "A" (Atom "ok") (Comm "A" "B" (Atom "res1") (Comm "B" "A" (Atom "ok") End))) (Comm "B" "A" (Atom "wrong") (Comm "A" "B" (Atom "res2") (Local "B" (PIf (BEq (Atom "X") (Atom "b")) (Comm "B" "A" (Atom "ok") End) (Comm "B" "A" (Atom "wrong") End)) End)))) End))
      let tokens = AL.alexScanTokens x
      let (name, sig0, sig, ags, roles, kn, cells, actions, bound) = AP.parseAnnB tokens -- TODO: analyze that cells used do exist????
      let header = S.getHeader sig0 sig ags roles kn
      let projs = A.actionsToProjs actions header
      -- putStrLn $ show projs
      let (transactions, c):_ = S.runMState (A.initConvert projs header) S.initialState
      let (newHeader, newCells) = A.updateHeaderAndCells c header cells
      let nn = G.generate newHeader newCells transactions bound
      putStrLn nn
    _ -> putStrLn "Wrong number of arguments"
 