module Main (
  main,
  compile
) where

import Prelude

import Control.Monad.Aff (Aff, Canceler, makeAff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, error, log)
import Control.Monad.Eff.Exception (message, throwException)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Uncurried (EffFn1, mkEffFn1)
import Control.Promise (Promise)
import Control.Promise as Promise
import DOM (DOM)
import Data.Either (either)
import Data.Function.Uncurried (Fn1, mkFn1)
import Network.HTTP.Affjax (AJAX)
import StoryParser.System (compileToConfig, loadGameAjax)
import StoryParser.Types (SerializedConfig(..))
import UI.Web as Web
import Mustache (MustacheEffect)

-- import UI.Console as Console

runAffAndShowErrors :: forall e a. Aff (console :: CONSOLE | e) a -> Eff (console :: CONSOLE | e) (Canceler (console :: CONSOLE | e))
runAffAndShowErrors = runAff (message >>> error) (const $ pure unit)

-- main = Console.main
main :: forall e. Eff (mustache :: MustacheEffect, ref :: REF, dom :: DOM , now :: NOW , random :: RANDOM , ajax :: AJAX , console :: CONSOLE | e) Unit
main = void $ runAffAndShowErrors $ do
  config <- loadGameAjax "game.json"
  liftEff $ log "Loaded config"
  liftEff $ Web.main config

-- | Takes a string, and returns a promise to compile
-- | the string into a SerializedConfig
compile :: forall e. EffFn1 e String (Promise SerializedConfig)
compile = mkEffFn1 $ \str ->
  Promise.fromAff $ makeAff (\err success ->
    either err success (compileToConfig str)
  )

-- | Takes a string, and returns a promise to compile
-- | the string into a SerializedConfig
-- compile :: forall e. String -> Eff e (Promise SerializedConfig)
-- compile str = Promise.fromAff $ makeAff (\err success -> either err success (compileToConfig str))