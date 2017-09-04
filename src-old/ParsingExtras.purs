module Text.Parsing.Extras where

import Prelude

import Control.Alt ((<|>))
import Data.Array (many, some)
import Data.Array as Array
import Data.Foldable (class Foldable)
import Data.String (fromCharArray)
import Text.Parsing.Parser (ParserT(..))
import Text.Parsing.Parser.Combinators (manyTill, optional)
import Text.Parsing.Parser.String (class StringLike, anyChar, char, skipSpaces)
import Text.Parsing.Parser.Token (alphaNum)

-- | Convert a foldable container of characters to a string
fromCharList :: forall f. Foldable f => f Char -> String
fromCharList = fromCharArray <<< Array.fromFoldable

-- | Parse a single alphanumeric string
alphaNumString :: forall m. Monad m => ParserT String m String
alphaNumString = fromCharList <$> some alphaNum

-- | Parse all characters between single or double quotes
betweenQuotes :: forall s m. (StringLike s) => Monad m => ParserT s m String
betweenQuotes = do
  q <- (char '"' <|> char '\'')
  t <- fromCharList <$> manyTill anyChar (char q)
  pure t

-- | Parse all characters between double quotes
betweenDoubleQuotes :: forall s m. (StringLike s) => Monad m => ParserT s m String
betweenDoubleQuotes = char '"' *> (fromCharList <$> manyTill anyChar (char '"'))

-- | Parse all characters between single quotes
betweenSingleQuotes :: forall s m. (StringLike s) => Monad m => ParserT s m String
betweenSingleQuotes = char '\'' *> (fromCharList <$> manyTill anyChar (char '\''))

-- | Parse an array of items embed within braces and
-- | seperated by commas. Whitespace is allowed between braces, items, and commas, e.g.
-- | [ whitespace (item) whitespace , whitespace (item) whitespace ]
parseArray :: forall s m a. (StringLike s) => (Monad m) => ParserT s m a -> ParserT s m (Array a)
parseArray fn = char '[' *>
  (many
    (skipSpaces *> fn <* skipSpaces <* (optional (skipSpaces *> char ',')))
  ) <*
  char ']'