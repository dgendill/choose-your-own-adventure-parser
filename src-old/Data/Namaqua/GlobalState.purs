module Data.Namaqua.GlobalState where

import GameBoilerplate
import MyPrelude

import Data.Lens.Lens (lens)
import Data.Lens.Setter (set)
import Data.Lens.Types (Lens', Setter)

type Game = GameState GlobalState
-- type CommandSelection e c = Ref GlobalState -> Eff (ref :: REF | e) (Commands c)

data Location
  = Shelter
  | ParkingLot
  | Playground
  | ModenaSign
  | Field
  | RiverBed
  | RiverBank
  | Bathroom


type ShelterState = Unit
type ParkingState = Unit
type PlaygroundState = Unit
type SignState = Unit
type FieldState = Unit
type RiverBedState = Unit
type RiverBankState = Unit
type BathroomState = Unit

newtype GlobalState = GlobalState {
  location :: Location,
  shelterState :: Ref ShelterState,
  parkingState :: Ref ParkingState,
  fieldState :: Ref FieldState,
  signState :: Ref SignState,
  playgroundState :: Ref PlaygroundState,
  riverBankState :: Ref RiverBankState,
  riverBedState :: Ref RiverBedState,
  bathroomState :: Ref BathroomState
}

location :: Lens' GlobalState Location 
location = lens (\(GlobalState r) -> r.location) (\(GlobalState r) l -> GlobalState (r { location = l}) )

setLocation :: Location -> GlobalState -> GlobalState
setLocation = set location

  
gGoLocation :: forall e. Location -> String -> Array String -> Ref GlobalState -> NiceCommand (ref :: REF | e)
gGoLocation loc text aliases gstate = {
  hidden: false,
  friendly : text,
  aliases : [text] <> aliases,
  exec : do
    modifyRef gstate (setLocation loc)
    pure $ ShadowCommand (AppCommand {
      friendly : text,
      raw : "Look Around"
    }) []
}


instance game1Def :: GameDef GlobalState where
  getGameCommands = defaultGetGameCommands



goRiverBed'' gstate s alias = gGoLocation RiverBed s ([s] <> alias <> ["Goto Riverbed"]) gstate
goRiverBed' gstate s = goRiverBed'' gstate s []
goRiverBed gstate = goRiverBed' gstate "Goto Riverbed"

goRiverBank'' gstate s alias = gGoLocation RiverBank s (alias <> ["Goto Riverbank"]) gstate
goRiverBank' gstate s = goRiverBank'' gstate s []
goRiverBank gstate = goRiverBank' gstate "Goto Riverbank"

goField' gstate s = gGoLocation Field s [s] gstate
goField gstate = gGoLocation Field "Explore Field" ["Explore Field"] gstate

goParking gstate = gGoLocation ParkingLot "Goto Parking Lot" ["Goto Parking Lot"] gstate

goSign'' gstate s alias = gGoLocation ModenaSign s (alias <> ["Goto Plaque", "Goto Grave"])  gstate
goSign' gstate s = goSign'' gstate s []
goSign gstate = goSign' gstate "Goto Grave"

goBathroom gstate = gGoLocation Bathroom "Goto Bathrooms" ["Goto Bathrooms"] gstate
goPlayground gstate = gGoLocation Playground "Goto Playground" ["Goto Playground"] gstate
goShelter gstate = gGoLocation Shelter "Goto Picnic Tables"
    ["Goto Picnic Tables", "go picnic", "go shelter", "shelter", "picnic", "walk to picnic tables"]
    gstate


derive instance genericLocation :: Generic Location
instance eqLocation :: Eq Location where eq = gEq
