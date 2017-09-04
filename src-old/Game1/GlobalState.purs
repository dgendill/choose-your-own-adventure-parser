module Data.Game1.GlobalState where

import GameBoilerplate
import MyPrelude
import Data.DateTime.Locale (LocalDateTime)

type Game = GameState GlobalState

data Location
  = Bedroom
  | Kitchen

newtype GlobalState = GlobalState {
  location :: Location,
  phoneState :: Ref PhoneState,
  bedroomState :: Ref BedroomState
}

type PhoneState = {
  passwords :: StrMap Boolean,
  on :: Boolean,
  gamestart :: LocalDateTime
}

type BedroomState = {
  examinedDresser :: Boolean
}

derive instance genericLocation :: Generic Location
instance eqLocation :: Eq Location where eq = gEq

instance game1Def :: GameDef GlobalState where
  getGameCommands = defaultGetGameCommands

