module Data.Namaqua.RiverBank where

import Data.Namaqua.GlobalState
import GameBoilerplate
import MyPrelude

look :: String
look = """
<p>You are on the bank of the Big Thompson River. It's a nice sunny day and the sound of the water sooths you.
Up the river, you can see people floating on inner tubes.</p>
<img src="img/riverbank.jpg">
"""

type CommandEffs e = (ref :: REF | e)

commands :: forall e c. Ref FieldState -> Ref GlobalState -> Eff (ref :: REF | e) (NiceCommands (CommandEffs c))
commands lstate gstate = makeNiceCommands [
  gLook $ pure $ TextContent look,
  goRiverBed' gstate "Back to Riverbed"
]

initialState :: Unit
initialState = unit