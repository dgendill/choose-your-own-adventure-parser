module Main where

import Prelude

import Control.Monad.Aff (Aff, Canceler, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, error, log)
import Control.Monad.Eff.Exception (message)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.Eff.Ref (REF)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)
import StoryParser.System (loadGameAjax)
import UI.Web as Web
-- import UI.Console as Console

runAffAndShowErrors :: forall e a. Aff (console :: CONSOLE | e) a -> Eff (console :: CONSOLE | e) (Canceler (console :: CONSOLE | e))
runAffAndShowErrors = runAff (message >>> error) (const $ pure unit)

-- main = Console.main
main :: forall e. Eff (ref :: REF, dom :: DOM , now :: NOW , random :: RANDOM , ajax :: AJAX , console :: CONSOLE | e) Unit
main = void $ runAffAndShowErrors $ do
  config <- loadGameAjax "game.json"
  liftEff $ log "Loaded config"
  liftEff $ Web.main config