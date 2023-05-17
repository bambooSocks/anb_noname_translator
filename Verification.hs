module Verification where

import qualified Control.Monad.Trans.State as ST

data Msg
  = Atom String
  | Composition String [Msg]
  deriving (Show, Eq)

type Frame = [(Msg, Msg)]

data FrameState = FrameState
  { counter :: Int
  , frame   :: Frame
  }
  deriving (Show)

initialFrameState :: FrameState
initialFrameState = FrameState 
  { counter = 0
  , frame = []
  }

type MState = ST.StateT FrameState []

get :: MState FrameState
-- fetch the current state
get = ST.get

put :: FrameState -> MState ()
-- update the state with new version
put = ST.put

runMState :: MState a -> FrameState -> [(a, FrameState)]
-- apply the state transformer on an state and return the final state and value
runMState = ST.runStateT

evalMState :: MState a -> FrameState -> [a]
-- apply the state transformer on an state and return the final value
evalMState = ST.evalStateT

execMState :: MState a -> FrameState -> [FrameState]
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
receive m@(Atom id) = do
    fresh m
-- pair receiving
receive m@(Composition "pair" (f:s:[])) = do
    fresh m
    receive f
    receive s
-- scrypt receiving has to have a key sk in frame to receive the msg
receive m@(Composition "scrypt" (sk:msg:[])) = do
    fresh m
    receiveIfInFrame sk msg
-- crypt receiving has to have a private key inv(pk) in frame to receive the msg
receive m@(Composition "crypt" (pk:msg:[])) = do
    fresh m
    receiveIfInFrame (Composition "inv" [pk]) msg
-- sign receiving has to have a public key pk in frame to receive the msg
receive m@(Composition "sign" ((Composition "inv" (pk:[])):msg:[])) = do
    fresh m
    receiveIfInFrame pk msg
-- fallback receive for other compositions
receive m@(Composition id args) = -- TODO: support custom deconstructors
    fresh m

main :: IO ()
main = do
  ---- pair example
  -- let x = Composition "pair" [Atom "a", Composition "pair" [Atom "b", Atom "c"]]
  ---- scrypt example
  -- let x = Composition "pair" [Atom "sk", (Composition "scrypt" [Atom "sk", Atom "secret"])]
  let x = Composition "pair" [(Composition "scrypt" [Atom "sk", Atom "secret"]), Atom "sk"]
  -- let x = Composition "scrypt" [Atom "sk", Atom "secret"]
  ---- crypt example
  -- let x = Composition "pair" [Composition "inv" [Atom "pk"], (Composition "crypt" [Atom "pk", Atom "secret"])]
  -- let x = Composition "crypt" [Atom "pk", Atom "secret"]
  ---- sign example
  -- let x = Composition "pair" [Atom "pk", (Composition "sign" [Composition "inv" [Atom "pk"], Atom "secret"])]
  -- let x = Composition "sign" [Composition "inv" [Atom "pk"], Atom "secret"]
  let mst = receive x
  let st = execMState mst initialFrameState
  putStrLn $ show st