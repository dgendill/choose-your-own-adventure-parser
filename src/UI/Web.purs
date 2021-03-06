module UI.Web where

import MyPrelude

import Control.Monad.Eff.Console (log)
import DOM.Event.Event (preventDefault, target)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.Types (EventType(..))
import DOM.HTML.HTMLInputElement (setValue)
import DOM.HTML.HTMLTextAreaElement (value)
import DOM.Node.Document (createElement)
import DOM.Node.Element (getAttribute, setAttribute)
import DOM.Node.Node (textContent)
import Data.Array (head, mapWithIndex, null)
import Data.StrMap as StrMap
import Data.String (joinWith)
import Data.Traversable (for, traverse)
import GameBoilerplate (AppCommand(AppCommand), GameState(GameState), NiceCommand, StorageKey(StorageKey), runCommand)
import HTML (document, prependChild, setDataAttribute, setInnerHTML, toElement, toEventTarget, toNode, unsafeGetElementById)
import Mustache (MustacheEffect, render)
import Storage (mkStore)
import StoryParser (initGame)
import StoryParser.Types (SerializedConfig)
import Unsafe.Coerce (unsafeCoerce)

type Effects e = (mustache :: MustacheEffect, now :: NOW, ref :: REF, random :: RANDOM , console :: CONSOLE, dom :: DOM | e)

setHTMLSuggestions :: forall e c. Array (NiceCommand c) -> Eff (Effects e) Unit
setHTMLSuggestions niceCommands = do 
  suggestionTemplate <- textContent =<< toNode <$> unsafeGetElementById "story-suggestion-template"
  eSuggestions <- unsafeGetElementById "suggestions"
  eSuggestionsWrapper <- unsafeGetElementById "suggestions-wrapper"

  lis <- for (mapWithIndex Tuple niceCommands) \(Tuple i command) -> do
    render suggestionTemplate (StrMap.fromFoldable [
      (Tuple "label" command.friendly),
      (Tuple "index" $ show (i+2))
    ]) StrMap.empty  

  setInnerHTML eSuggestions (joinWith " " lis)


main :: forall e. SerializedConfig -> Eff (Effects e) Unit
main config = do
  storyTemplate <- textContent =<< toNode <$> unsafeGetElementById "story-template"
  suggestionTemplate <- textContent =<< toNode <$> unsafeGetElementById "story-suggestion-template"
  
  eCommandArea <- unsafeGetElementById "commandArea"
  eSuggestions <- unsafeGetElementById "suggestions"
  eContent <- unsafeGetElementById "content"
  eTextarea <- unsafeGetElementById "textarea"
  eSuggestionsWrapper <- unsafeGetElementById "suggestions-wrapper"



  game@(GameState {
    state : gameRef,
    commands : commands,
    content : content,
    storageKey : (StorageKey storageKey)
  }) <- initGame config (StorageKey "namaqua")

  let storage = mkStore storageKey
    
  -- runCommand :: AppCommand -> Eff (Effects e) {success, text, command}
  let
    webRunCommand = runCommand game
      (\ac s commands -> do
        setHTMLSuggestions commands.niceCommands
        pure {
          success : true,
          text : s,
          command : ac
        }    
    ) (\ac@(AppCommand appCommand) -> pure $ {
      success : false,
      text : "Hmmm... I'm not sure what you mean. <!-- raw:" <> appCommand.raw <> ", friendly: " <> appCommand.friendly <> "-->",
      command : ac
    })

  -- execCommand :: forall e. String -> Eff (Effects e) AppCommand
  let
    execCommand commandText = do
      { success
      , text
      , command : (AppCommand command)
      } <- webRunCommand (AppCommand { friendly: commandText, raw: commandText})

      r <- render storyTemplate (StrMap.fromFoldable [
        (Tuple "command" command.friendly),
        (Tuple "content" text)
      ]) StrMap.empty

      setValue "" (unsafeCoerce eTextarea)      
      setInnerHTML eContent r
      pure success

  let execAndSaveCommand command = execCommand command >>= case _ of
        true -> storage.save command
        false -> pure unit

  storage.removeSequentialDuplicates
  oldCommands <- storage.getAll
  if null oldCommands
    then void $ execAndSaveCommand "Look Around"
    else void $ for oldCommands execCommand
  
  
  addEventListener (EventType "submit") (eventListener (\event -> do
    preventDefault event
    command <- value (unsafeCoerce eTextarea)
    execAndSaveCommand command
  )) false (toEventTarget eCommandArea)

  addEventListener (EventType "click") (eventListener (\event -> do
      preventDefault event
      getAttribute "data" (toElement $ target event) >>= case _ of
        Just command -> execAndSaveCommand command          
        Nothing -> pure unit
  )) false (toEventTarget eSuggestionsWrapper)


  log "Running"