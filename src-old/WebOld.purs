module UI.WebOld where

import MyPrelude

import Control.Monad.Eff.Console (log)
import DOM.Event.Event (preventDefault, target)
import DOM.Event.EventTarget (addEventListener, eventListener)
import DOM.Event.Types (EventType(..))
import DOM.HTML.HTMLInputElement (setValue)
import DOM.HTML.HTMLTextAreaElement (value)
import DOM.Node.Document (createElement)
import DOM.Node.Element (getAttribute, setAttribute)
import Data.Array (head, mapWithIndex, null)
import Data.Traversable (for, traverse)
import GameBoilerplate (AppCommand(AppCommand), GameState(GameState), NiceCommand, StorageKey(StorageKey), runCommand)
import HTML (document, prependChild, setDataAttribute, setInnerHTML, toElement, toEventTarget, unsafeGetElementById)
import Storage (mkStore)
import StoryParser (initGame)
import StoryParser.Types (SerializedConfig)
import Unsafe.Coerce (unsafeCoerce)

type Effects e = (now :: NOW, ref :: REF, random :: RANDOM , console :: CONSOLE, dom :: DOM | e)

setHTMLSuggestions :: forall e c. Array (NiceCommand c) -> Eff (Effects e) Unit
setHTMLSuggestions niceCommands = do
  suggestionsWrapper <- unsafeGetElementById "suggestions-wrapper"
  ul <- document >>= createElement "ul"  
  lis <- for (mapWithIndex Tuple niceCommands) \(Tuple i command) -> do
    li <- document >>= createElement "li"
    button <- document >>= createElement "button"
    prependChild li button
    setInnerHTML button command.friendly
    setDataAttribute button (fromMaybe "" (head command.aliases))
    setAttribute "tabindex" (show (i+2)) button

    pure li
  void $ traverse (prependChild ul) lis
  setInnerHTML suggestionsWrapper ""
  prependChild suggestionsWrapper ul

main :: forall e. SerializedConfig -> Eff (Effects e) Unit
main config = do
  suggestionsWrapper <- unsafeGetElementById "suggestions-wrapper"
  commandArea <- unsafeGetElementById "commandArea"
  textarea <- unsafeGetElementById "textarea"
  contentElement <- unsafeGetElementById "content"
  
  log "ok"

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

      setValue "" (unsafeCoerce textarea)
      commandElement <- document >>= createElement "b"
      commandOutput <- document >>= createElement "div"
      setAttribute "class" "fadein" commandOutput
      setInnerHTML commandElement command.friendly
      setInnerHTML commandOutput text 
      void $ prependChild contentElement commandOutput
      void $ prependChild contentElement commandElement
      log text
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
    command <- value (unsafeCoerce textarea)
    execAndSaveCommand command
  )) false (toEventTarget commandArea)

  addEventListener (EventType "click") (eventListener (\event -> do
      preventDefault event
      getAttribute "data" (toElement $ target event) >>= case _ of
        Just command -> execAndSaveCommand command          
        Nothing -> pure unit
  )) false (toEventTarget suggestionsWrapper)


  log "Running"