module Data.Namaqua.Shelter (
  commands,
  initialState
) where

import MyPrelude
import Data.Namaqua.GlobalState
import GameBoilerplate

initialState :: Unit
initialState = unit

type CommandEffs e = (ref :: REF | e)

commands :: forall e c. Ref ShelterState -> Ref GlobalState -> Eff (ref :: REF | e) (NiceCommands (CommandEffs c))
commands lstate gstate = makeNiceCommands [
  gLook look,
  goPlayground gstate,
  goField' gstate "Explore Field",
  goParking gstate
]

look :: forall e. Eff e CommandResult
look = pure $ TextContent """
<p>You take a seat at one of the six picnic tables.
There is a playground to your right and an open field to the left.
There are two grills here and you can see a horseshoe pit nearby.
</p>
<img src="img/picnic-tables.jpg">
"""