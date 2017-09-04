module Data.Namaqua.Field where

import MyPrelude
import GameBoilerplate
import Data.Namaqua.GlobalState

look :: String
look = """
<p>You explore the field and find shade trees and picnic benches.
One of the benches is etched "Juggalos." You notice a pathway in the
tall grass and there is a playground behind you. </p>
<img src="img/field.jpg">
"""

type CommandEffs e = (ref :: REF | e)

commands :: forall e c. Ref FieldState -> Ref GlobalState -> Eff (ref :: REF | e) (NiceCommands (CommandEffs c))
commands lstate gstate = makeNiceCommands [
  gLook $ pure $ TextContent look,
  goRiverBed' gstate "Follow Pathway",
  goPlayground gstate,
  goShelter gstate
]


initialState :: Unit
initialState = unit