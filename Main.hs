module Main where

import System.Environment
import qualified Analysis as A
import qualified AnnB_Lexer as AL
import qualified AnnB_Parser as AP

main :: IO ()
main = do
  args <- getArgs
  case args of 
    [file] -> do
      x <- readFile file
      let tokens = AL.alexScanTokens x
      let (name, sig0, sig, ags, know, cells, actions, bound) = AP.parseAnnB tokens
      let proj = A.actionsToProjs actions
      -- let trans = A.translate 
      putStrLn $ show (proj)
    _ -> putStrLn "Wrong number of arguments"

