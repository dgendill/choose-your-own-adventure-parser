module Data.Game1.Bedroom where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, Ref)
import Data.StrMap (StrMap, fromFoldable)
import Data.Tuple (Tuple(..))
import Data.Game1.GlobalState (GlobalState, BedroomState)
import GameBoilerplate

initialState :: forall e. Eff (ref :: REF | e) BedroomState
initialState = pure {
  examinedDresser : false
}

commands :: forall e c. Ref BedroomState -> Ref GlobalState -> Eff e (Commands c)
commands s globalState = pure $ fromFoldable $
  (Tuple <$> ["look"] <*> (pure start))
  -- <>
  -- (Tuple <$> ["examine dresser", "goto dresser"] <*> (pure $ examinePhone s)) <>
  
start :: forall e. Eff e CommandResult
start = pure $ TextContent """
You are sitting your bedroom.  There is a dresser is in the corner, and your bed is positioned diagonally
in the center of the room.  You know this is your room, but for some reason you can't remember who you are,
how you got here, and why things are the way they are.
"""

