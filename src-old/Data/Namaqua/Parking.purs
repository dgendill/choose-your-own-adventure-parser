module Data.Namaqua.Parking (
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

commands :: forall e c. Ref ParkingState -> Ref GlobalState -> Eff (ref :: REF | e) (NiceCommands (CommandEffs c))
commands lstate gstate = makeNiceCommands [
  gLook look,
  goShelter gstate,
  goBathroom gstate
  
]

look :: forall e. Eff e CommandResult
look = pure $ TextContent """
<p>You are at Namaqua park. You notice a sign that says, "Closes at 10:30.  No alcoholic beverages allowed." You can see
a playground and a shelter with picnic tables under it. On the far side of the parking lot are two portable bathrooms.</p>
<img src="img/parking.jpg">
"""

