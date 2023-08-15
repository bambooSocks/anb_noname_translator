module State where

import qualified Control.Monad.Trans.State as ST
import qualified Data.Map as Map
import Data.Foldable (for_)
import Data.Char (isUpper)
import Types (Label, Msg, Agent, Marking(ToDo, Done), Frame, Def, SigmaDef (Public, Private), AgentDef (Honest, Dishonest))

data Header = Header
  { s0    :: [Def]
  , sPub  :: [Def]
  , sPriv :: [Def]
  , hAgs  :: [Agent]
  , dAgs  :: [Agent]
  }
  deriving(Show)

getHeader :: [Def] -> [SigmaDef] -> [AgentDef] -> Header
getHeader sig0 sig ags = do
  let (sPub, sPriv) = splitSigmaDef sig ([],[])
  let (hAgs, dAgs) = splitAgentDef ags ([],[])
  Header { s0    = sig0
  , sPub  = sPub
  , sPriv = sPriv
  , hAgs  = hAgs
  , dAgs  = dAgs
  }

data State = State
  { counter    :: Int
  , frame      :: Frame
  , pubFunc    :: [(String, Int)]
  , cellLabels :: [Label]
  , xVars      :: [String]
  , yVars      :: [String]
  }
  deriving (Show)

initialState :: State
initialState = State 
  { counter    = 0
  , frame      = Map.empty
  , pubFunc    = [("pair", 2), ("crypt", 3), ("scrypt", 3), ("sign", 2)]
  , cellLabels = []
  , xVars      = []
  , yVars      = []
  }

splitSigmaDef :: [SigmaDef] -> ([Def], [Def]) -> ([Def], [Def])
splitSigmaDef [] (sPub, sPriv) =
  (sPub, sPriv)
splitSigmaDef ((Public d):sds) (sPub, sPriv) =
  (sPub ++ d, sPriv)
splitSigmaDef ((Private d):sds) (sPub, sPriv) =
  (sPub, sPriv ++ d)

splitAgentDef :: [AgentDef] -> ([Agent], [Agent]) -> ([Agent], [Agent])
splitAgentDef [] (hAgs, dAgs) =
  (hAgs, dAgs)
splitAgentDef ((Honest a):ads) (hAgs, dAgs) = 
  (hAgs ++ a, dAgs)
splitAgentDef ((Dishonest a):ads) (hAgs, dAgs) = 
  (hAgs, dAgs ++ a)

isPublicId :: String -> Int -> State -> Bool
-- check whether the identifier is public with correct arity
isPublicId id arity s = do
  any (\(i,a) -> i == id && a == arity) (pubFunc s)

freshLabel :: Agent -> State -> (Label, State)
 -- generate a fresh label
freshLabel ag s = do
  let i = counter s
  let label = ag ++ "_X" ++ show i
  (label, (s { counter = i + 1 }))

register :: Msg -> Marking -> Label -> State -> State
-- register a new label and corresponding message in the frame
register msg marking label s = do
  let newFrame = Map.insert label (msg, marking) (frame s)
  s { frame = newFrame }

registerFresh :: Agent -> Msg -> Marking -> State -> (Label, State)
-- generate a new label and register message with given marking
registerFresh ag msg marking s = do
  let (label, s1) = freshLabel ag s
  let s2 = register msg marking label s1
  (label, s2)

registerManyFresh :: Agent -> [Msg] -> Marking -> State -> ([Label], State)
-- generate a new label and register message with given marking
registerManyFresh _ [] _ s =
  ([], s)
registerManyFresh ag (msg:msgs) marking s = do
  let (label, s1) = freshLabel ag s
  let s2 = register msg marking label s1
  let (rest, s3) = registerManyFresh ag msgs marking s2
  (label:rest, s3)

addInitialState :: Agent -> Header -> [Msg] -> State -> State
addInitialState ag h msgs s = do
  let (_, s1) = registerManyFresh ag msgs ToDo s
  let ags = map (\a -> (a, 0)) ((hAgs h) ++ (dAgs h))
  s1 { pubFunc = ((s0 h) ++ (sPub h) ++ ags ++ (pubFunc s1)) }

addToXVars :: Label -> State -> State
addToXVars var s =
  s { xVars = (var:(xVars s)) }

clearXVars :: State -> State
clearXVars s =
  s { xVars = [] }

setYVars :: [Label] -> State -> State
setYVars yVars s =
  s { yVars = yVars }