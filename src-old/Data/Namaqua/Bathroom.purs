module Data.Namaqua.Bathroom (
  commands,
  initialState
) where

import Data.Namaqua.GlobalState
import GameBoilerplate
import MyPrelude

import Control.Monad.Eff.Ref (modifyRef)

initialState :: Unit
initialState = unit

type CommandEffs e = (ref :: REF | e)

commands :: forall e c. Ref BathroomState -> Ref GlobalState -> Eff (ref :: REF | e) (NiceCommands (CommandEffs c))
commands lstate gstate = makeNiceCommands [
  gLook look,
  goSign' gstate "Examine Grave",
  goParking gstate
]

look :: forall e. Eff e CommandResult
look = pure $ TextContent """
<p>You walk over to the bathrooms. To your right you see what looks like a grave stone.</p>
<img src="img/bathrooms.jpg">
"""