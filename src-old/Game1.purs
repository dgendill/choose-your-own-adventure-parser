module Data.Game1 (
  init,
  currentCommands
) where

import Data.Game1.GlobalState
import GameBoilerplate
import MyPrelude

import Control.Monad.Eff.Ref (newRef)
import Data.Game1.Bedroom as Bedroom
import Data.Game1.Phone as Phone

type Effects e = (
  now :: NOW,
  ref :: REF,
  random :: RANDOM,
  console :: CONSOLE,
  dom :: DOM | e
)

init :: forall e. Eff (Effects e) Game
init = do
  phoneState <- Phone.initialState >>= newRef
  bedroomState <- Bedroom.initialState >>= newRef
  state <- newRef $ GlobalState {
    location : Bedroom,
    phoneState,
    bedroomState
  }
  pure $ GameState {
    storageKey : "commands",
    state
  }

gameCommands :: forall e c
   . Ref GlobalState
  -> Eff (ref :: REF | e) (Array (Eff (ref :: REF | e) (NiceCommands (Effects c))))
gameCommands state = do
  (GlobalState s) <- readRef state
  case s.location == Bedroom of
    true -> do
      pure $ [
        Phone.phoneCommands s.phoneState state,
        Bedroom.commands s.bedroomState state
      ]
    false -> do
      pure $ [
        Phone.phoneCommands s.phoneState state
      ]

currentCommands :: forall e c
   . Ref GlobalState
  -> Eff (ref :: REF | e) (NiceCommands (Effects c))
currentCommands state = getGameCommands state gameCommands

