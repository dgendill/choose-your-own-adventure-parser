module Data.Namaqua (
  init,
  currentCommands
) where

import Data.Namaqua.GlobalState
import GameBoilerplate
import MyPrelude

import Control.Monad.Eff.Ref (newRef)
import Data.Namaqua.Shelter as Shelter
import Data.Namaqua.Parking as Parking
import Data.Namaqua.Field as Field
import Data.Namaqua.ModenaSign as ModenaSign
import Data.Namaqua.Playground as Playground
import Data.Namaqua.RiverBank as RiverBank
import Data.Namaqua.RiverBed as RiverBed
import Data.Namaqua.Bathroom as Bathroom

type Effects e = (
  now :: NOW,
  ref :: REF,
  random :: RANDOM,
  console :: CONSOLE,
  dom :: DOM | e
)

init :: forall e. Eff (Effects e) Game
init = do
  shelterState <- newRef Shelter.initialState
  parkingState <- newRef Parking.initialState
  fieldState <- newRef Field.initialState
  signState <- newRef ModenaSign.initialState
  playgroundState <- newRef Playground.initialState
  riverBankState <- newRef RiverBank.initialState
  riverBedState <- newRef RiverBed.initialState
  bathroomState <- newRef Bathroom.initialState

  state <- newRef $ GlobalState {
    location : ParkingLot,
    shelterState,
    parkingState,
    fieldState,
    signState,
    playgroundState,
    riverBankState,
    riverBedState,
    bathroomState
  }
  pure $ GameState {
    storageKey : "namaqua",
    state
  }

currentCommands :: forall e c
   . Ref GlobalState
  -> Eff (ref :: REF | e) (NiceCommands (Effects c))
currentCommands state = getGameCommands state gameCommands

gameCommands :: forall e c
   . Ref GlobalState
  -> Eff (ref :: REF | e) (Array (Eff (ref :: REF | e) (NiceCommands (Effects c))))
gameCommands state = do
  (GlobalState s) <- readRef state
  case s.location of
    Shelter -> pure $ [ Shelter.commands s.shelterState state ]
    ParkingLot -> pure $ [ Parking.commands s.parkingState state ]
    Field -> pure $ [ Field.commands s.fieldState state ]
    ModenaSign -> pure $ [ ModenaSign.commands s.signState state ]
    Playground -> pure $ [ Playground.commands s.playgroundState state ]
    RiverBank -> pure $ [ RiverBank.commands s.riverBankState state ]
    RiverBed -> pure $ [ RiverBed.commands s.riverBedState state ]
    Bathroom -> pure $ [ Bathroom.commands s.bathroomState state ]

