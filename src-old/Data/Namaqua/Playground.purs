module Data.Namaqua.Playground where

import Data.Namaqua.GlobalState
import GameBoilerplate
import MyPrelude

look :: String
look = """
<p>You are standing next to the playground.  There are two regular swings, two baby swings,
and a tire swing here.  Some kids are playing nearby on the jungle gym which has
monkey bars and two slides. Next door is <a target="blank" href="http://www.lrmconcrete.com/">Loveland Ready-Mix</a> concrete plant.
You see a grave and some picnic tables to your right. An open field is behind you.
</p>
<img src="img/playground.jpg">
"""

type CommandEffs e = (ref :: REF | e)

commands :: forall e c. Ref FieldState -> Ref GlobalState -> Eff (ref :: REF | e) (NiceCommands (CommandEffs c))
commands lstate gstate = makeNiceCommands [
  gLook $ pure $ TextContent look,
  goSign' gstate "Investigate Grave",
  goField' gstate "Explore Field",
  goShelter gstate
]

initialState :: Unit
initialState = unit