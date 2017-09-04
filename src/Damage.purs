module Damage where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.Eff.Ref (REF, Ref, readRef)
import Control.Monad.ST (ST, STRef, readSTRef)
import Data.Int (round, toNumber)

type SharedItem e = { name :: String | e}

data Item
  = Weapon (SharedItem ( damage :: Int ))
  | Thing (SharedItem ())

newtype Monster = Monster
  { commonItem :: Array Item
  , rareItem :: Array Item
  }

newtype Character = Character
  { level :: Number
  , stamina :: Number
  , strength :: Number
  , speed :: Number
  , magic :: Number
  }
-- char stats
-- stamina (stamina)
-- vigor   (strength)
-- speed   (agility)
-- magic power
-- evade
-- blockPropability


-- effects
-- Sleep, Petrify, Freeze, ResistantTo (effect)

charMagicAttack :: Number -> Number -> Number -> Number
charMagicAttack spellPower level magicPower = spellPower * 4.0 + (level * magicPower * spellPower / 32.0)

monsterMagicAttack :: Number -> Number -> Number -> Number
monsterMagicAttack spellPower level magicPower = spellPower * 4.0 + (level * (magicPower * 3.0 / 2.0) * spellPower / 32.0)

randomEncounter :: forall e. Ref Int ->  Eff (ref :: REF , random :: RANDOM | e ) Boolean
randomEncounter countRef = do
  count <- readRef countRef
  r <- randomInt 0 255
  pure $ if r < round (toNumber(count) / 256.0)
    then true
    else false


-- A fun battle is...
-- The right balance on the continuam of easy and hard (depending on context)
-- Options for fighting.  Variation such that the player feels their decisions are meaningful and effective.  Creates a sense of curiosity
-- ( *what if* I did this instead?)
-- Creates a pathway for both a "bad player" and a "excellent player"

data Direction
  = North
  | South
  | East
  | West

data Action
  = Climb
  | TimeForward
  | TimeBackward

data GameF
  = Go Direction
  | RunAction Action



--- examineDresser
--- goDresser
--- state+= drawerOpen,itemsVisible
openDresser = """
  You open the bottom drawers and they are empty, the top drawer contains
  Steven King's "The Library Policeman" and a glass pen.  Sitting on top of
  the dresser is a cell phone.
"""

--examineBook
--examineTheLongWalk
--examineRichardBachmanBook
examineBook = """
  You don't know why you own this book, but a part of you remembers that you've been meaning to read it.
"""

-- state+=penNowDevice
examinePen = """
  On closer inspection you discover it's not a pen.  The glass tube has a blue button the side and a dial
  that can be turned so it points to the words "Forward" or "Back".
"""


--getPen
--getCellPhone
--getCellPhone
--getBook
--getPen
--getGlassPen
--getItems
getItems = """
  You pick up the items and put them in your briefcase.
"""

inventory = """
  Your briefcase contains nothing.
  Your briefcase contains:

  - Glass Pen
  - "The Long Walk"
  - Cell Phone
"""

dropItem = "You contemplate dropping the _ but decide it's better to keep it."
