module UI.Console where

import GameBoilerplate
import MyPrelude

import Control.Monad.Aff (Aff, makeAff, runAff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (error)
import Control.Monad.Eff.Console as EC
import Control.Monad.Eff.Exception (message)
import Control.Monad.Rec.Class (forever)
import Data.String (trim)
import Node.ReadLine (Interface, READLINE, close, createConsoleInterface, noCompletion, prompt, setLineHandler, setPrompt)
import StoryParser (initGame, initGameFrom)

doNothing = const $ pure unit

main = runAff (\err -> do
    error $ message err
  ) doNothing do

  gs  <- initGameFrom "./content"

  let
    runGameCommand = runCommand gs
      (\ac@(AppCommand command) s commands -> do
        pure {
          success : true,
          text : s,
          command : ac
        }    
    ) (\ac@(AppCommand appCommand) -> pure $ {
      success : false,
      text : "Hmmm... I'm not sure what you mean.",
      command : ac
    })

  -- execCommand :: forall e. String -> Eff (Effects e) AppCommand
  let
    execCommand commandText = do
      {success, text, command : (AppCommand command)} <- runGameCommand (AppCommand { friendly: commandText, raw: commandText})
      EC.log $ " - " <> command.friendly <> " -"
      EC.log (trim text)
      EC.log ""
      pure success

  void $ liftEff $ execCommand "Look Around"
  
  forever $ do
    i <- liftEff $ createConsoleInterface noCompletion 
    input <- affQuestion "What next? > " i
    liftEff $ EC.log ""
    void $ liftEff $ execCommand input
    liftEff $ close i


affQuestion :: forall e. String -> Interface -> Aff (readline :: READLINE | e) String
affQuestion q i = makeAff (\onError onSuccess -> do
  setPrompt q 0 i
  setLineHandler i onSuccess
)