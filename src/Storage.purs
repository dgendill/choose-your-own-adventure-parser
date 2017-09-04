module Storage (mkStore) where

import Prelude

import Control.Monad.Eff (Eff)
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Window (localStorage)
import DOM.WebStorage.Storage (Storage)
import DOM.WebStorage.Storage as S
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode.Class (class DecodeJson, decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Array (find, group, nubBy, snoc)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.NonEmpty (head)
import Data.String (toLower, trim)

ls :: forall e. Eff (dom :: DOM | e) Storage
ls = window >>= localStorage

toString' :: forall a. (EncodeJson a) => a -> String
toString' = encodeJson >>> stringify

fromString' :: forall a. (DecodeJson a) => String -> Either String a
fromString' = jsonParser >=> decodeJson

--lskey :: String
--lskey = "commands"

type Store e = String -> Eff (dom :: DOM | e) Unit
type GetMany e = Eff (dom :: DOM | e) (Array String)
type Noop e = Eff (dom :: DOM | e) Unit

mkStore :: forall e. String -> {
  save :: Store e,
  getAll :: GetMany e,
  removeSequentialDuplicates :: Noop e }
mkStore lskey = {
  save : mkStoreCommand lskey,
  getAll : mkGetStoredCommands lskey,
  removeSequentialDuplicates : removeSequentialDuplicates lskey
}

mkStoreCommand :: forall e a. String -> (String -> Eff (dom :: DOM | e) Unit)
mkStoreCommand lskey command = ls >>= \storage -> do
  commands <- mkGetStoredCommands lskey
  S.setItem lskey (toString' (snoc commands command)) storage

mkGetStoredCommands :: forall e. String -> Eff (dom :: DOM | e) (Array String)
mkGetStoredCommands lskey = ls >>= \storage -> do
  s <- S.getItem lskey storage
  let commands = s # maybe [] (either (const []) id <<< fromString')
  pure commands

removeSequentialDuplicates :: forall e. String -> Eff (dom :: DOM | e) Unit
removeSequentialDuplicates lskey =  ls >>= \storage -> do
  commands <- mkGetStoredCommands lskey
  let cleaned = map head (group commands) -- nubBy (\a b -> trim (toLower a) == trim (toLower b)) commands
  S.setItem lskey (toString' cleaned) storage