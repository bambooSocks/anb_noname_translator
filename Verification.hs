module Verification where

import qualified Control.Monad.Trans.State as ST
import qualified Data.Map as M

data Msg
  = Atom String
  | Comp String [Msg]
  deriving (Show, Eq)

type Frame = [(Msg, Msg)]
type AgentFrames = M.Map String Frame

data State = State
  { counter :: Int
  , frame   :: Frame
  , frames  :: AgentFrames -- TODO: convert the rest to support agent specific frames
  , pubFunc :: [String]
  }
  deriving (Show)

initialFrameState :: State
initialFrameState = State 
  { counter = 0
  , frame   = []
  , frames  = M.fromList []
  , pubFunc = ["pair", "crypt", "scrypt", "sign"]
  }

type MState = ST.StateT State []

get :: MState State
-- fetch the current state
get = ST.get

put :: State -> MState ()
-- update the state with new version
put = ST.put

runMState :: MState a -> State -> [(a, State)]
-- apply the state transformer on an state and return the final state and value
runMState = ST.runStateT

evalMState :: MState a -> State -> [a]
-- apply the state transformer on an state and return the final value
evalMState = ST.evalStateT

execMState :: MState a -> State -> [State]
-- apply the state transformer on an state and return the final state
execMState = ST.execStateT

register :: String -> Msg -> MState Int
-- register a new label and corresponding message in the frame
register name msg = do
  state <- get
  let i = counter state
  let f = frame state
  put state { counter = i + 1
            , frame = (Atom name, msg) : f
            }
  pure i

fresh :: Msg -> MState Int
-- generate a fresh name for a new 
fresh msg = do
  state <- get
  let name = "x" ++ show (counter state)
  register name msg

receiveIfInFrame :: Msg -> Msg -> MState Int
-- receive the second msg if the first one is in the frame
receiveIfInFrame cond_msg rcv_msg = do
  state <- get
  let fr = map snd (frame state)
  if any (cond_msg ==) fr then
        receive rcv_msg
    else
        return (counter state)

receive :: Msg -> MState Int
-- Atom receiving
receive m@(Atom _) = do
    fresh m
-- pair receiving
receive m@(Comp "pair" (f:s:[])) = do
    fresh m
    receive f
    receive s
-- scrypt receiving has to have a key sk in frame to receive the msg
receive m@(Comp "scrypt" (sk:msg:[])) = do
    fresh m
    receiveIfInFrame sk msg
-- crypt receiving has to have a private key inv(pk) in frame to receive the msg
receive m@(Comp "crypt" (pk:msg:[])) = do
    fresh m
    receiveIfInFrame (Comp "inv" [pk]) msg
-- sign receiving has to have a public key pk in frame to receive the msg
receive m@(Comp "sign" ((Comp "inv" (pk:[])):msg:[])) = do
    fresh m
    receiveIfInFrame pk msg
-- fallback receive for other compositions
receive m@(Comp id args) = -- TODO: support custom deconstructors
    fresh m

canDeduceFromFrame :: Msg -> MState Bool
-- checks whether an atom can be deduced from the frame
canDeduceFromFrame msg@(Atom _) = do
  state <- get
  let fr = map snd (frame state)
  return (any (msg ==) fr)
-- checks whether a composition can be deduced from the frame
canDeduceFromFrame msg@(Comp id args) = do
  state <- get
  let pubFuncs = pubFunc state
  let fr = map snd (frame state)
  let isInFrame =  (any (msg ==) fr)
  let idAllowed = any (id ==) pubFuncs
  -- TODO: clean this up
  let allArgsCanBeDeduced = all (\a -> all (True ==) (evalMState (canDeduceFromFrame a) state)) args
  return (isInFrame || (idAllowed && allArgsCanBeDeduced))

main :: IO ()
main = do
  ---- pair example
  -- let x = Comp "pair" [Atom "a", Comp "pair" [Atom "b", Atom "c"]]
  ---- scrypt example
  -- let x = Comp "pair" [Atom "sk", (Comp "scrypt" [Atom "sk", Atom "secret"])]
  -- let x = Comp "pair" [(Comp "scrypt" [Atom "sk", Atom "secret"]), Atom "sk"]
  -- let x = Comp "scrypt" [Atom "sk", Atom "secret"]
  ---- crypt example
  -- let x = Comp "pair" [Comp "inv" [Atom "pk"], (Comp "crypt" [Atom "pk", Atom "secret"])]
  -- let x = Comp "crypt" [Atom "pk", Atom "secret"]
  ---- sign example
  -- let x = Comp "pair" [Atom "pk", (Comp "sign" [Comp "inv" [Atom "pk"], Atom "secret"])]
  -- let x = Comp "sign" [Comp "inv" [Atom "pk"], Atom "secret"]
  ---- test for deducing scrypt(sk, secret)
  let x = Comp "pair" [Atom "secret", Atom "sk"]
  let mst = receive x
  let st:[] = execMState mst initialFrameState
  let canDeduce = evalMState (canDeduceFromFrame (Comp "scrypt" [Atom "sk", Atom "secret"])) st
  putStrLn $ show st
  putStrLn $ show canDeduce