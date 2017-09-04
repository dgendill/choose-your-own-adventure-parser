module Data.Namaqua.RiverBed where

import Data.Namaqua.GlobalState
import GameBoilerplate
import MyPrelude

look :: String
look = """
<p>You hike through the tall grass and find yourself in a dried river bed.
It looks like someone has been stacking rocks.
There are many pathways to explore, and you can see the river
through the tall foliage.</p>
<img src="img/riverbed.jpg">
"""

type CommandEffs e = (ref :: REF | e)

commands :: forall e c. Ref FieldState -> Ref GlobalState -> Eff (ref :: REF | e) (NiceCommands (CommandEffs c))
commands lstate gstate = makeNiceCommands [
  gLook $ pure $ TextContent look,
  goRiverBank gstate,
  goField' gstate "Back to Field"
]

initialState :: Unit
initialState = unit