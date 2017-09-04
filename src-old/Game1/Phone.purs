module Data.Game1.Phone where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff, whileE)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Now (NOW)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, readRef)
import Control.Monad.ST (ST, STRef, modifySTRef, newSTRef, readSTRef, runST)
import Data.Array (catMaybes, index, length, mapWithIndex, range)
import Data.Array.ST (STArray, emptySTArray, freeze, peekSTArray, pokeSTArray, pushSTArray, runSTArray, thaw, unsafeFreeze)
import Data.Array.ST.Iterator (Iterator, iterator)
import Data.Array.Shuffle (shuffle)
import Data.DateTime.Locale (LocalDateTime)
import Data.EuclideanRing (mod)
import Data.Foldable (find, findMap)
import Data.Function (applyFlipped)
import Data.Maybe (Maybe(..))
import Data.Ord (lessThan, lessThanOrEq, max)
import Data.Semiring (add)
import Data.StrMap (StrMap, filter, foldMaybe, fromFoldable, insert, keys, thawST, union)
import Data.Traversable (for)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Game1.GlobalState (GlobalState, PhoneState)
import Time (clockDisplay, now)
import GameBoilerplate

type CommandEffs e = (now :: NOW, ref :: REF, random :: RANDOM | e) 

initialState :: forall e. Eff (now :: NOW | e) PhoneState
initialState = do
  n <- now
  pure {
    passwords : (union dumbPasswordsAlways dumbPasswordsRandom),
    on : true,
    gamestart : n
  }

phoneCommands :: forall e c. Ref PhoneState -> Ref GlobalState -> Eff e (Commands (CommandEffs c))
phoneCommands s globalState = pure $ fromFoldable $
  (Tuple <$> ["examine phone", "examine cell phone", "examine cellphone"] <*> (pure $ examinePhoneCommand s)) <>
  (Tuple <$> ["unlock phone", "unlock cell phone", "unlock cellphone", "enter password"] <*> (pure $ unlockPhoneCommand s)) <>
  (Tuple <$> ["turn on phone", "turn on cell phone", "turn on cellphone"] <*> (pure $ turnOnPhoneCommand s)) <>
  (Tuple <$> ["turn off phone", "turn off cell phone", "turn off cellphone"] <*> (pure $ turnOffPhoneCommand s))
 


-- phoneTime :: forall e. Ref PhoneState -> Eff (now :: NOW | e) String
-- phoneTime state = do
--   s <- readRef state
--   n <- now

tryPasswordRef :: forall e. Ref PhoneState -> Eff (random :: RANDOM, ref :: REF | e) (Maybe String)
tryPasswordRef ref = do
  state <- readRef ref
  result <- tryPassword state
  case result of
    Just pass -> do
      modifyRef ref (\s -> s {
        passwords = insert pass true state.passwords
      })
      pure result
    Nothing -> pure result


tryPassword :: forall e. PhoneState -> Eff (random :: RANDOM, ref :: REF | e) (Maybe String)
tryPassword state = do
  -- state <- readRef s
  dpa <- shuffle (keys dumbPasswordsAlways)
  dpr <- shuffle (keys dumbPasswordsRandom)
  let unusedPasswords = keys $ filter (eq false) state.passwords
  
  let mresult = findMap (\key -> (find (eq key) dpa)) unusedPasswords <|>
                findMap (\key -> (find (eq key) dpr)) unusedPasswords

  case mresult of
    Just result -> do
      
      pure mresult
    Nothing -> pure mresult


dumbPasswordsAlways :: StrMap Boolean
dumbPasswordsAlways = fromFoldable $
  map (\v -> Tuple v false)
    ["query123", "password", "password123", "admin", "pass"]

dumbPasswordsRandom :: StrMap Boolean
dumbPasswordsRandom = fromFoldable $
  map (\v -> Tuple v false)
    ["666666", "admin", "abc123", "twitter", "server"]

toCommand :: forall e. (Ref PhoneState -> Eff (now :: NOW, ref :: REF| e) String) -> Ref PhoneState -> Eff (now :: NOW, ref :: REF| e) CommandResult
toCommand fn s = TextContent <$> fn s

examinePhoneCommand :: forall e. Ref PhoneState -> Eff (now :: NOW, ref :: REF| e) CommandResult
examinePhoneCommand = toCommand examinePhoneText

examinePhoneText :: forall e. Ref PhoneState -> Eff (now :: NOW, ref :: REF| e) String
examinePhoneText state = do
  s <- readRef state
  case s.on of
    true -> do
      n <- now
      pure $ examineCellPhoneOnText (clockDisplay n)
    false -> pure $ examineCellPhoneOffText

-- if state=phoneOn
examineCellPhoneOnText :: String -> String
examineCellPhoneOnText gameTime = """
It is a Moto G without a case.  You touch to unlock the phone, and see it is password protected. You go
to the home screen and see """ <> gameTime <> """ followed by Adrian Filbert adbert@hotmail.com.
"""

-- if state=phoneOff
examineCellPhoneOffText :: String
examineCellPhoneOffText = """
It is a Moto G without a case.  It is turned off right now.
"""

turnOffPhoneCommand = toCommand turnOffPhone

turnOffPhone :: forall e. Ref PhoneState -> Eff (now :: NOW, ref :: REF, random :: RANDOM | e) String
turnOffPhone state = do
  ifPhoneOn state (do
    modifyRef state (_ { on = false })
    pure """
You press and hold the button on the side and tap the familiar "Power Off" confirmation.  The screen goes blank.
"""
  ) "The phone is already turned off."

turnOnPhoneCommand = toCommand turnOnPhone

turnOnPhone :: forall e. Ref PhoneState -> Eff (now :: NOW, ref :: REF, random :: RANDOM | e) String
turnOnPhone state = do
  text <- ifPhoneOn state (do
    examine <- examinePhoneText state
    pure $ "The phone is already turned on. " <> examine
  ) """
You press and hold the button on the side and the screen lights up displaying "powered by android".  You continue to
wait, and then see the home screen.
"""
  modifyRef state (_ { on = true })
  pure text

ifPhoneOn :: forall e. Ref PhoneState -> Eff (ref :: REF, random :: RANDOM | e) String -> String -> Eff (ref :: REF, random :: RANDOM | e) String
ifPhoneOn state fn offText = do
  s <- readRef state
  case s.on of
    true -> fn
    false -> pure offText

unlockPhoneCommand = toCommand unlockPhone

unlockPhone :: forall e. Ref PhoneState -> Eff (ref :: REF, random :: RANDOM | e) String
unlockPhone state = do
  s <- readRef state
  ifPhoneOn state (do
    tryPasswordRef state >>= pure <<< case _ of
      Just text -> unlockPhoneText text
      Nothing -> "You try to think of other possible password but give up. There's no point in trying more."
  ) "The phone is off right now, so you can't unlock it."
  where
  unlockPhoneText :: String -> String
  unlockPhoneText password = """
  You don't know the password, but you decide to try <i>""" <> password <> """</i>.  It doesn't work.
  """

