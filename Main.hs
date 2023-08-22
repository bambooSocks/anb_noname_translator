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
      let tokens = AL.alexScanTokens x
      let (name, sig0, sig, ags, roles, kn, cells, actions, bound) = AP.parseAnnB tokens
      let header = S.getHeader sig0 sig ags roles kn
      let projs = A.actionsToProjs actions header
      let (transactions, c):_ = S.runMState (A.initConvert projs header) S.initialState
      let (newHeader, newCells) = A.updateHeaderAndCells c header cells
      let nn = G.generate newHeader newCells transactions bound
      putStrLn nn
    _ -> putStrLn "Wrong number of arguments"
 