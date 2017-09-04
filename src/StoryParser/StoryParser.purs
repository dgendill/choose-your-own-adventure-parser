module StoryParser where

-- import GameBoilerplate
import Prelude
import StoryParser.System
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Aff.Console (logShow)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Ref (REF, Ref, newRef)
import Data.Array (head, tail)
import Data.Either (Either(Right))
import Data.Foldable (foldMap)
import Data.Lens.Getter (view)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.StrMap (StrMap, empty, fromFoldable, lookup, singleton)
import Data.Tuple (Tuple(Tuple))
import GameBoilerplate (CommandResult(..), Game, GameState(..), GlobalState(..), NiceCommand, StorageKey(..), gGoLocation, gLook)
import Node.FS (FS)
import Node.Path (FilePath)
import StoryParser.Types (ActionType(Go), Command(Command), CommandAlias(ExactAlias, AliasGroup), GameConfig(GameConfig), ParsedStory, SerializedConfig(SerializedConfig), _commands, _content, _name)

-- | Given an aff computation that returns showable data,
-- | run the computation and show the data.
debug :: forall a e. (Show a) => Aff (console :: CONSOLE | e) a -> Eff (exception :: EXCEPTION, console :: CONSOLE | e) Unit
debug fn = void $ launchAff $ do
  r <- fn
  logShow r

-- | Create a new game from a serialized configuration and a storage key
initGame :: forall e e2. SerializedConfig -> StorageKey -> Eff (ref :: REF | e) (Game (ref :: REF | e))
initGame (SerializedConfig sConfig) storageKey = do
  let config = sConfig.config
  let stories = sConfig.stories

  state <- newRef $ GlobalState {
    location : fromMaybe "" $ (view _name <$> head stories),
    locations :  fromFoldable $ map (\r -> Tuple (view _name r) empty) stories
  }

  let commands = getCommands state stories config

  pure $ GameState {
    state : state,
    commands : commands,
    content : getStoriesContent (map Right stories),
    storageKey : storageKey
  }

-- | Given a global state, an array of stories, and a game config,
-- | return a mapping from user-define location names to
-- | commands that are available at that location.
getCommands :: forall e. Ref GlobalState -> Array ParsedStory -> GameConfig -> (StrMap (Array (NiceCommand (ref :: REF | e))))
getCommands state stories config@(GameConfig uConfig) = do
  foldMap (\r ->
    singleton
      (view _name r)
      ((toNiceCommands (view _commands r))
        <> [gLook (pure $ TextContent $ view _content r)]
      )  
  ) stories
  where

  -- Convert an array of command aliases and actions into an array of
  -- UI commands
  toNiceCommands :: forall eff. Array Command -> Array (NiceCommand (ref :: REF | eff))
  toNiceCommands = foldMap (\(Command {commandAliases, action}) ->
    case action of 
      Go location ->
        let commands = (toExactAliasesOnly commandAliases) <#> aliasToString
            rest = fromMaybe [] (tail commands)
        in case head commands of
          Just first -> [gGoLocation location first rest state]
          _ -> []             
  )

  -- Given an array of CommandAliases, replace all AliasGroups with
  -- their cooresponding ExactAliases.
  toExactAliasesOnly :: Array CommandAlias -> Array CommandAlias
  toExactAliasesOnly commands = foldMap (case _ of
    AliasGroup g -> case lookup g uConfig.aliasGroups of
      Just commandAliases -> commandAliases
      _ -> [ExactAlias "Failure"]
    a -> [a]
  ) commands

  -- Turn a CommandAlias into a string
  aliasToString :: CommandAlias -> String
  aliasToString = case _ of
    AliasGroup g -> g
    ExactAlias a -> a

-- | Given a path to a folder, load a game directly
initGameFrom :: forall e e2. FilePath -> Aff (console :: CONSOLE, fs :: FS, ref :: REF | e) (Game (ref :: REF | e2))
initGameFrom path = do
  result <- getStories path
  config@(GameConfig uConfig) <- getConfigFile path >>= eitherToAff  
  let locations = getFileNames result

  state <- liftEff $ newRef $ GlobalState {
    location : uConfig.startLocation,
    locations : locationsToStateMap locations
  }

  let commands = getCommands state (toParsedStories result) config

  pure $ GameState {
    state : state,
    commands : commands,
    content : getStoriesContent result,
    storageKey : (StorageKey "consoleGame")
  }
  where
    locationsToStateMap :: Array String -> StrMap (StrMap String)
    locationsToStateMap locations =
      fromFoldable $ map (\name -> Tuple name empty) locations