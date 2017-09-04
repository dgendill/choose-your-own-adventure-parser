module StoryParser.Parser (
    parseStory,
    parseManyStories,
    parseIndexedStory,
    parseIndexedMultipleStories,
    parseConfig
  ) where

import Prelude
import Text.Parsing.Extras (alphaNumString, betweenQuotes, fromCharList, parseArray)
import Control.Alt ((<|>))
import Control.Monad.Error.Class (catchError)
import Data.Array (many, some)
import Data.Either (Either)
import Data.StrMap (fromFoldable)
import Data.Tuple (Tuple(Tuple))
import Node.Path (FilePath)
import StoryParser.Types (ActionType(Go), Command(Command), CommandAlias(ExactAlias, AliasGroup), GameConfig(GameConfig), ParsedStory(ParsedStory), StoryParser)
import Text.Parsing.Parser (ParseError, fail, runParser)
import Text.Parsing.Parser.Combinators (choice, manyTill)
import Text.Parsing.Parser.String (anyChar, char, eof, skipSpaces, string)
import Text.Parsing.Parser.Token (alphaNum)

-- | Given a string attempt to parse a story
parseStory :: String -> Either ParseError ParsedStory
parseStory str = runParser str parseStory'

-- | Given a string attempt to parse many stories
parseManyStories :: String -> Either ParseError (Array ParsedStory)
parseManyStories str = runParser str parseManyStories'

-- | Parse an array of command aliases
parseCommandAliases :: StoryParser (Array CommandAlias)
parseCommandAliases = parseArray (
    AliasGroup <$> alphaNumString
    <|> ExactAlias <$> betweenQuotes
    <|> fail "Could not parse AliasGroup or ExactAlias."
  )

-- | Parse a game action
parseActionType :: StoryParser ActionType
parseActionType = string "go" *> skipSpaces *> (Go <$> alphaNumString)

-- | Parse a game command
parseCommand :: StoryParser Command
parseCommand = map Command $ {
  commandAliases : _,
  action : _
  }
  <$> parseCommandAliases <* skipSpaces <*> parseActionType

-- | Parse a story
parseStory' :: StoryParser ParsedStory
parseStory' = map ParsedStory $ {
    contentType : _,
    name : _,
    commands : _,
    content : _
  }
  <$> 
  (skipSpaces *> (catchError
        (choice [string "Text"])
        (\e -> fail $ "Expected 'Text' at beginning of file")
  ))
  <*  skipSpaces
  <*> (catchError
        (fromCharList <$> (some alphaNum))
        (\e -> fail $ "Expected name of story after 'Text'")
      )
  <* skipSpaces
  <*> many (parseCommand <* skipSpaces)
  <* skipSpaces <* string "---"
  <*> (fromCharList <$> (manyTill anyChar (eof <|> (map (const unit) $ string "---next---"))))


parseManyStories' :: StoryParser (Array ParsedStory)
parseManyStories' = many parseStory'

-- | Given an Tuple associating a file to a string, attempt to parse a story.
-- | On failure show the error an which file could not be parsed.
parseIndexedStory :: Tuple FilePath String -> Either ParseError ParsedStory
parseIndexedStory (Tuple file str) =
  catchError (runParser str do
    parseStory'
  ) (\e -> runParser "" $ fail $ "Could not parse '" <> file <> "'! " <> (show e))

parseIndexedMultipleStories :: Tuple FilePath String -> Either ParseError (Array ParsedStory)
parseIndexedMultipleStories (Tuple file str) =
  catchError (runParser str do
    parseManyStories'
  ) (\e -> runParser "" $ fail $ "Could not parse '" <> file <> "'! " <> (show e))


-- | Parse a game configuration
parseConfig :: StoryParser GameConfig
parseConfig =
  map GameConfig $ {
    startLocation : _,
    aliasGroups : _
  }
  <$> parseStartLocation <*> (fromFoldable <$> parseAliasGroups)
  where
  parseStartLocation =
    skipSpaces *> string "startAt" *>
    skipSpaces *> alphaNumString <* char ';'
  parseAliasGroups = many
    (
      skipSpaces *> string "aliasGroup" *>
      skipSpaces *> (Tuple <$> alphaNumString) <*
      skipSpaces <* string "as" <* skipSpaces <*> parseManyExactCommands <* char ';'
    )

-- | Parse many exact command aliases
parseManyExactCommands :: StoryParser (Array CommandAlias)
parseManyExactCommands = parseArray (ExactAlias <$> betweenQuotes)
