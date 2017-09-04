module Data.Namaqua.ModenaSign where

import Data.Namaqua.GlobalState
import GameBoilerplate
import MyPrelude

look :: String
look = """
You're standing in front of the Modena family graves.
There is a plaque that reads:

<pre style="text-align: center;white-space: pre-line;border: 1px solid #666;padding: 20px;">
        DEDICATED TO THE MEMORY OF
            C. C. BUCKINGHAM
              1846 - 1940

    WHOSE ESTATE FULFILLED HIS DESIRE TO
    PROVIDE THIS RESTING PLACE FOR THE

              MODENA FAMILY
        FIRST SETTLERS OF NAMAQUA
      
        ERECTED BY NAMAQUA CHAPTER
    DAUGHTERS OF THE AMERICAN REVOLUTION
              JULY 21, 1960
</pre>

<p>On the ground are markers for "Baby Boy",
"Mariano Modena", "Lena Modena", "Marie John Modena" and "A Friend".

The bathrooms are to your left.  Behind you is the playground.</p>
<img src="img/mariano.jpg">



"""

type CommandEffs e = (ref :: REF | e)

commands :: forall e c. Ref FieldState -> Ref GlobalState -> Eff (ref :: REF | e) (NiceCommands (CommandEffs c))
commands lstate gstate = makeNiceCommands [
  gLook $ pure $ TextContent look,
  goBathroom gstate,
  goPlayground gstate
  -- goParking gstate
]

initialState :: Unit
initialState = unit