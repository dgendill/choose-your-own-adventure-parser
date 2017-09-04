module StoryParser.Types where

import GameBoilerplate hiding (Command)
import Prelude

import Data.Either (Either)
import Data.Foreign.Class (class Decode, class Encode)
import Data.Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens.Lens (lens)
import Data.Lens.Types (Lens')
import Data.Newtype (class Newtype)
import Data.StrMap (StrMap)
import Text.Parsing.Parser (ParseError, Parser)

-- | Alias for the context in which we'll
-- | parse the game definition.
type StoryParser a = Parser String a
type StoryParserResult = Array (Either ParseError ParsedStory)

-- | The types of actions we'll allow in the game.
data ActionType = Go Location
derive instance gActionType :: Generic ActionType _
instance showActionType :: Show ActionType where show (Go s) = "Go " <> s
instance decodeActionType :: Decode ActionType where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeActionType :: Encode ActionType where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

-- | A command can either be entered as an exact string
-- | or reference a "alias group", which is basically a
-- | variable for an array of string litersals.
data CommandAlias
  = AliasGroup String
  | ExactAlias String
derive instance gCommandAlias :: Generic CommandAlias _
instance showCommandAlias :: Show CommandAlias where show = genericShow
instance decodeCommandAlias :: Decode CommandAlias where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeCommandAlias :: Encode CommandAlias where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

-- | A Command will change the state of the game
-- | and is triggered by text.
newtype Command = Command {
  action :: ActionType,
  commandAliases :: Array CommandAlias
}
derive instance gCommand :: Generic Command _
instance showCommand :: Show Command where show = genericShow
instance decodeCommand :: Decode Command where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeCommand :: Encode Command where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

-- | Defines starting location and command aliases
-- | for the game.
newtype GameConfig = GameConfig {
  startLocation :: String,
  aliasGroups :: StrMap (Array CommandAlias)
}
derive instance gGameConfig :: Generic GameConfig _
instance showGameConfig :: Show GameConfig where show = genericShow
instance decodeGameConfig :: Decode GameConfig where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeGameConfig :: Encode GameConfig where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}

-- | Each file will be parsed into a ParsedStory which
-- | contains the type of content, a name, and the commands
-- | that can be executed.
newtype ParsedStory = ParsedStory {
  contentType :: String,
  name :: String,
  commands :: Array Command,
  content :: String
}
derive instance gParsedStory :: Generic ParsedStory _
instance decodeJsonStory :: Decode ParsedStory where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeJsonStory :: Encode ParsedStory where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
instance showParsedStory :: Show ParsedStory where show = genericShow
derive instance newtypeParsedStory :: Newtype ParsedStory _

-- | After parsing the config and story files, they will be
-- | stored in this data, which can be encoded and decoded to
-- | json.
newtype SerializedConfig = SerializedConfig {
  config :: GameConfig,
  stories :: Array ParsedStory
}
derive instance gSerializedConfig :: Generic SerializedConfig _
instance decodeSerializedConfig :: Decode SerializedConfig where
  decode = genericDecode $ defaultOptions {unwrapSingleConstructors = true}
instance encodeSerializedConfig :: Encode SerializedConfig where
  encode = genericEncode $ defaultOptions {unwrapSingleConstructors = true}
instance showSerializedConfig :: Show SerializedConfig where show = genericShow
derive instance newtypeSerializedConfig :: Newtype SerializedConfig _

-- | Get the name field off of a ParsedStory
_name :: Lens' ParsedStory String
_name = lens (\(ParsedStory s) -> s.name) (\(ParsedStory s) v -> ParsedStory (s { name = v }))

-- | Get the content field off of a ParsedStory
_content :: Lens' ParsedStory String
_content = lens (\(ParsedStory s) -> s.content) (\(ParsedStory s) v -> ParsedStory (s { content = v }))

-- | Get the commands field off of a ParsedStory
_commands :: Lens' ParsedStory (Array Command)
_commands = lens (\(ParsedStory s) -> s.commands) (\(ParsedStory s) v -> ParsedStory (s { commands = v }))

-- | Get the contentType field off of a ParsedStory
_contentType :: Lens' ParsedStory String
_contentType = lens (\(ParsedStory s) -> s.contentType) (\(ParsedStory s) v -> ParsedStory (s { contentType = v }))
