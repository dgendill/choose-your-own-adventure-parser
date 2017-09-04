module GameBoilerplate (
    Commands,
    Command,
    NiceCommands,
    NiceCommand,
    AppCommand,
    AppCommand(..),
    GameState,
    GameState(..),
    CommandResult,
    CommandResult(..),
    class GameDef,
    defaultGetGameCommands,
    getGameCommands,
    makeCommands,
    makeNiceCommands,
    gLook,
    gGoLocation,
    normalizeString,
    hideNiceCommand,
    GlobalState,
    GlobalState(..),
    Location,
    StorageKey,
    StorageKey(..),
    Game,
    currentCommands,
    runCommand,
    runCommand'
  ) where
  
import Control.Monad.Eff (kind Effect)
import Control.Monad.Eff.Ref (REF, Ref, readRef)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Data.Foldable (foldMap)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (genericDecode, genericEncode, defaultOptions)
import Data.Generic.Rep (class Generic, Constructor(..))
import Data.Generic.Rep.Eq (genericEq)
import Data.Lens.Lens (lens)
import Data.Lens.Setter (set)
import Data.Lens.Types (Lens')
import Data.Maybe (fromMaybe)
import Data.Newtype (class Newtype)
import Data.StrMap (StrMap, empty, lookup, union)
import Data.String (toLower, trim)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import MyPrelude hiding ((#))
import Unsafe.Coerce (unsafeCoerce)

-- | The result of a command can be either text
-- | or another command that the caller can choose
-- | to run.
data CommandResult
  = TextContent String
  | ShadowCommand AppCommand (Array CommandResult)

-- | When a command is executed via text, it may not
-- | be formatted nicely for a UI.  This type allows
-- | commands to hold the raw entered text, as well as
-- | a representation to be used in UIs.
newtype AppCommand = AppCommand {
  friendly :: String,
  raw :: String
}
derive instance genericAppCommand :: Generic AppCommand _
instance eqAppCommand :: Eq AppCommand where eq = genericEq
instance decodeAppCommand :: Decode AppCommand where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeAppCommand :: Encode AppCommand where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

-- | A game's state contains the storage key which allows
-- | command history to be saved, as well as a reference to the
-- | GlobalState
newtype GameState state e = GameState {
  storageKey :: StorageKey,
  content :: StrMap String,
  commands :: StrMap (Array (NiceCommand e)),
  state :: Ref state
}
derive instance newtypeGameState :: Newtype (GameState state e) _

-- | A type alias for GameState GlobalState
type Game e = GameState GlobalState e

-- | Since locations are defind in user-land,
-- | we'll define Locations as strings.
type Location = String

-- | State is defined by the location where the player is currently at,
-- | and the states of all the other locations in the game.
newtype GlobalState = GlobalState {
  location :: Location,
  locations :: StrMap (StrMap String)
}

-- | A wrapper around strings that reperesent keys in
-- | localStorage.
newtype StorageKey = StorageKey String
derive instance newtypeStorageKey :: Newtype StorageKey _

-- | A type alias represeting the UI element of a command.
type NiceCommand e = {
  hidden :: Boolean,
  friendly :: String,
  aliases :: Array String,
  exec :: Command e
}

-- | A type alias holding an array of UI elements and
-- | a mapping from text commands to effects.
type NiceCommands e = {
  niceCommands :: Array (NiceCommand e),
  commands :: Commands e 
}

-- | Type alias for an effectful function that returns
-- | the result of a command.
type Command e = Eff e CommandResult

-- | Type alias for a mapping from text commands to
-- | effectful functions.
type Commands e = StrMap (Command e)

currentCommands :: forall effs e. Game effs -> Eff (ref :: REF | e) (NiceCommands effs)
currentCommands (GameState { state, content, commands }) = do
  (GlobalState { location, locations }) <- readRef state
  let locationState = fromMaybe empty $ lookup location locations
  makeNiceCommands $ fromMaybe [] $ lookup location commands
  -- [
  --   gLook $ pure $ TextContent (fromMaybe "" $ (lookup location content))
  --   -- goPlayground gstate,
  --   -- goField' gstate "Explore Field",
  --   -- goParking gstate
  -- ]    

  

mappendNiceCommands :: forall e. NiceCommands e -> NiceCommands e -> NiceCommands e
mappendNiceCommands nc1 nc2 = {
  niceCommands : nc1.niceCommands <> nc2.niceCommands,
  commands : union (nc1.commands) (nc2.commands)
}

memptyNiceCommand :: forall e. NiceCommands e
memptyNiceCommand = {
  niceCommands : [],
  commands : empty
}

normalizeString :: String -> String
normalizeString = trim <<< toLower

makeCommands :: forall e c. Array (Tuple String (Command c)) -> Eff e (Commands c)
makeCommands = pure <<< fromFoldable

niceCommandToCommands :: forall e c. NiceCommand c -> Array (Tuple String (Command c))
niceCommandToCommands {aliases, exec} = Tuple <$> (map normalizeString aliases) <*> pure exec

makeNiceCommands :: forall e c. Array (NiceCommand c) -> Eff e (NiceCommands c)
makeNiceCommands niceCommands = do
  commands <- makeCommands $ foldMap niceCommandToCommands niceCommands
  pure {
    niceCommands : niceCommands,
    commands : commands
  }

hideNiceCommand :: forall e. NiceCommand e -> NiceCommand e
hideNiceCommand nc = nc { hidden = true }

gLook :: forall e. Command e -> NiceCommand e
gLook look = {
  hidden : false,
  friendly : "Look Around",
  aliases : ["Look Around", "Look", "Where Am I"],
  exec : look
}

class GameDef state where
  getGameCommands :: forall e c.
    Ref state
    -> (Ref state -> Eff (ref :: REF | e) (Array (Eff (ref :: REF | e) (NiceCommands c))))
    -> Eff (ref :: REF | e) (NiceCommands c)

instance game1Def :: GameDef GlobalState where
  getGameCommands = defaultGetGameCommands

defaultGetGameCommands :: forall c e a
   . Ref a
  -> (Ref a -> Eff (ref :: REF | e) (Array (Eff (ref :: REF | e) (NiceCommands c))))
  -> Eff (ref :: REF | e) (NiceCommands c)
defaultGetGameCommands state commandFn = do
  niceCommands <- commandFn state
  arr <- sequence niceCommands
  pure $ foldl (\acc arr -> mappendNiceCommands acc arr ) memptyNiceCommand arr


location :: Lens' GlobalState Location 
location = lens (\(GlobalState r) -> r.location) (\(GlobalState r) l -> GlobalState (r { location = l}) )

setLocation :: Location -> GlobalState -> GlobalState
setLocation = set location

-- | Create a command that will transfer the player to  
-- | a new location and look around. Under the hood, the command to trigger the transfer
-- | is the same ("Look Around" at the new location), but the UI element will be
-- | displayed differently depending on the friendly text passed in.
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

type CommandOutput e = Eff (ref :: REF | e) ({ success :: Boolean, text :: String, command :: AppCommand })
type CommandSuccessCb e = AppCommand -> CommandResult -> NiceCommands (ref :: REF | e) -> CommandOutput e
type CommandSuccessTextCb e = AppCommand -> String -> NiceCommands (ref :: REF | e) -> CommandOutput e
type CommandFailCb e = AppCommand -> CommandOutput e

runCommand' :: forall e. Game (ref :: REF | e) -> CommandSuccessCb e -> CommandFailCb e -> (AppCommand -> CommandOutput e)
runCommand' gs success fail = \ac@(AppCommand appCommand) -> do
  nc@{commands, niceCommands} <- currentCommands gs
  case lookup (normalizeString appCommand.raw) commands of
    Just fn -> do
      result <- fn
      success ac result nc
    Nothing -> fail ac


runCommand :: forall e. Game (ref :: REF | e) -> CommandSuccessTextCb e -> CommandFailCb e -> (AppCommand -> CommandOutput e)
runCommand gs success fail = runCommand' gs (\ac result commands ->
  case result of
    TextContent s -> do
      success ac s commands
    ShadowCommand command next -> do
      (runCommand gs success fail) command
) fail