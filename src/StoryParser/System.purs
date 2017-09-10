module StoryParser.System where

import Prelude

import Control.Monad.Aff (Aff, attempt, makeAff)
import Control.Monad.Eff.Exception (Error, error, message)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExcept)
import Control.Plus (empty)
import DOM (DOM)
import DOM.Node.Node (textContent)
import Data.Array (catMaybes, foldMap, zip)
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Bifunctor as BF
import Data.Either (Either(Right, Left), either, isLeft, note)
import Data.Foldable (fold)
import Data.Foreign (ForeignError, renderForeignError)
import Data.Foreign.Generic (defaultOptions, genericDecodeJSON, genericEncodeJSON)
import Data.Function (applyFlipped)
import Data.Lens.Getter (view)
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Newtype (class Newtype, unwrap)
import Data.Profunctor (rmap)
import Data.StrMap (StrMap, fromFoldable)
import Data.String.Utils (endsWith)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple(..), snd)
import HTML (getElementById, toNode)
import Network.HTTP.Affjax (AJAX, get)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (FS, readTextFile, readdir, writeTextFile)
import Node.Path (FilePath, sep)
import StoryParser.Parser (parseConfig, parseIndexedMultipleStories, parseIndexedStory, parseManyStories)
import StoryParser.Types (GameConfig(..), ParsedStory, SerializedConfig(SerializedConfig), StoryParserResult, _content, _name)
import Text.Parsing.Parser (ParseError(..), parseErrorMessage, parseErrorPosition, runParser)

-- | The result of a file system operation.
newtype FSResult a = FSResult a
derive instance newtypeFSResult :: Newtype (FSResult a) _

eitherToAff :: forall e a. Either Error a -> Aff e a
eitherToAff = case _ of
  Left err -> throwError err
  Right r -> pure r

path :: String
path = "./content"

file :: String
file = path <> "/game.json"

newtype From a = From a
derive instance newtypeFrom :: Newtype (From a) _
derive instance functorFrom :: Functor From
newtype To a = To a
derive instance newtypeTo :: Newtype (To a) _
derive instance functorTo :: Functor To

-- | Parse the files in a folder and
-- | serialize them into a json file.
saveGame :: forall e. From FilePath -> To FilePath -> Aff (fs :: FS | e) (FSResult String)
saveGame (From from) (To to) = do
  let toFile = to <> "/game.json"
  stories <- toParsedStories <$> getStories from
  config  <- getConfigFile from >>= eitherToAff
  writeTextFile UTF8 toFile
    (encode $ SerializedConfig {config, stories})
  pure $ FSResult $ "Serialized game saved to '" <> toFile <> "'"
  where encode = genericEncodeJSON defaultOptions

-- | Parse a single file and make the assumption that the first
-- | story part is the starting point.
saveAdventureGame :: forall e. From FilePath -> To FilePath -> Aff (fs :: FS | e) (FSResult String)
saveAdventureGame (From from) (To to) = do
  story <- readTextFile UTF8 from
  case compileToConfig story of
    Left err -> throwError $ error $ "Error in '" <> from <> "': " <> (message err)
    Right config -> do
      writeTextFile UTF8 toFile (encode config)
      pure $ FSResult $ "Serialized game saved to '" <> toFile <> "'"
  where
  encode = genericEncodeJSON defaultOptions
  toFile = to <> "/game.json"

-- | Compile a string and return the SerializedConfig
-- | or an error.
compileToConfig :: String -> Either Error SerializedConfig
compileToConfig story = do
  stories <- lmap parseErrorToError $ parseManyStories story
  firstStory <- note noStoryPartsError (Array.head stories)
  pure $ SerializedConfig {
      config : GameConfig {
          startLocation : (view _name firstStory),
          aliasGroups : mempty
      },
      stories : stories
    }
  where
  noStoryPartsError =
    error $ "No story parts could be parsed from the input."

parseErrorToError :: ParseError -> Error
parseErrorToError err = error $
  parseErrorMessage err <> ". " <>
  (show $ parseErrorPosition err)

  -- where
  -- stories = parseManyStories story

saveGameDefault  :: forall e. To FilePath -> Aff (fs :: FS | e) (FSResult String)
saveGameDefault = saveGame (From path)

-- | Load the serialized game from the default
-- | location
loadGame :: forall e. Aff (fs :: FS | e) SerializedConfig
loadGame =  readTextFile UTF8 file >>= decodeConfig

-- | Load the serialized game via ajax
loadGameAjax :: forall e. String -> Aff (ajax :: AJAX | e) SerializedConfig
loadGameAjax path = get path <#> _.response >>= decodeConfig

loadGameInline :: forall e. String -> Aff (dom :: DOM | e) SerializedConfig
loadGameInline id = decodeConfig =<< makeAff \err success -> do
  getElementById id >>= case _ of
    Just e -> success =<< textContent (toNode e)
    Nothing -> err $ error $ "Could not find element with id '" <> id <> "'."
    

-- | Decode the serialized game into a data type
decodeConfig :: forall e. String -> Aff e SerializedConfig
decodeConfig s = 
  case (runExcept $ decode s) of
    Right s -> pure s
    Left err ->
      throwError $ error $
        "Could not deserialize '" <> file <> "'. " <> (renderManyForeignError err)
  where decode = genericDecodeJSON defaultOptions

-- | Turn many ForeignErrors into a String
renderManyForeignError :: NonEmptyList ForeignError -> String
renderManyForeignError err = fold (renderForeignError <$> Array.fromFoldable err)

-- | Get the content of all .str files in a particular folder and
-- | return an Array of Tuples associate filenames to content
getMatchedContent :: forall e. FilePath -> Aff (fs :: FS | e) (Array (Tuple FilePath String))
getMatchedContent path = do
  files <- Array.filter (endsWith ".str") <$> readdir path
  content <- traverse (applyFlipped (path <> sep) append >>> readTextFile UTF8) files
  pure $ zip files content

-- | Get the content of all .str files in a particular folder
getContent :: forall e. FilePath -> Aff (fs :: FS | e) (Array String)
getContent path = map snd <$> getMatchedContent path

-- | Given a folder, attempt to read the Story.config file and return
-- | the GameConfig data type
getConfigFile :: forall e. FilePath -> Aff (fs :: FS | e) (Either Error GameConfig)
getConfigFile path = do
  let configPath = path <> "/Story.config"
  econtent <- attempt $ readTextFile UTF8 configPath
  pure $ case econtent of
    Right content -> case runParser content parseConfig of
        Left err -> Left $ error $
          "Error parsing " <> configPath
          <> ": " <> (parseErrorMessage err)
          <> " at " <> (show $ parseErrorPosition err)
        Right gameConfig -> Right gameConfig
    Left err -> Left $ error $ "Could not find config file " <> configPath <> "."

-- | Given a folder, attempt to read and then parse all .str files
getStories :: forall e. FilePath -> Aff (fs :: FS | e) StoryParserResult
getStories path =  Array.concatMap flattenStories <$> map parseIndexedMultipleStories <$> getMatchedContent path

-- | After parsing multiple stories from a single file, convert them into
-- | a standard StoryParserResult
flattenStories :: Either ParseError (Array ParsedStory) -> Array (Either ParseError ParsedStory)
flattenStories = sequence
-- (case e of
--   Left err -> [Left err]
--   Right r -> map Right r

unflattenStories :: StoryParserResult -> Either ParseError (Array ParsedStory)
unflattenStories = sequence

-- | Given a set of parsed stories, construct a StrMap associating
-- | the user-defined story name to the content.  Ignore any stories
-- | that had parser errors.
getStoriesContent :: StoryParserResult -> StrMap String
getStoriesContent result = fromFoldable $ map (case _ of
    Right r -> Tuple (view _name r) (view _content r)
    Left err -> Tuple "" ""
  ) result

-- | Get the user-defind names for a set of parsed stories
getFileNames :: StoryParserResult -> Array String
getFileNames f = map (either (const "") id) $ map (BF.rmap $ unwrap >>> _.name) f

-- | Get the stories that failed to parse
getFileStoriesErr :: forall e. FilePath -> Aff (fs :: FS | e) StoryParserResult
getFileStoriesErr path = map (Array.filter isLeft) $ getStories path

-- | Get the correctly parsed stories from a set of parsed stories,
-- | ignore stories that couldn't be parsed.
toParsedStories :: StoryParserResult -> Array ParsedStory
toParsedStories r = catMaybes $ (either (const Nothing) Just) <$> r
