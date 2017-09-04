module CommandLine where

import FSExtra
import Prelude

import Control.Monad.Aff (launchAff, runAff)
import Control.Monad.Aff.Class (liftAff)
import Control.Monad.Aff.Console as AC
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Console as EC
import Control.Monad.Eff.Exception (EXCEPTION, Error, error, message)
import Control.Monad.Except (runExcept)
import Data.Array (foldMap, head)
import Data.Either (Either(..))
import Data.Foreign (Foreign, readString)
import Data.Generic.Rep (class Generic, Argument(Argument), Constructor(Constructor), Sum(Inr, Inl))
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Newtype (unwrap)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Node.Buffer (BUFFER)
import Node.FS (FS)
import Node.FS.Aff (exists)
import Node.Path (sep)
import Node.Yargs.Applicative (rest, runY, yarg)
import StoryParser.System (FSResult(..), From(..), To(..), saveAdventureGame, saveGame)

-- | Define a console command which will be run
-- | when the user types in command
yargsCommand :: forall e
   . String
  -> Eff (console :: CONSOLE , exception :: EXCEPTION | e) Unit
  -> Eff (console :: CONSOLE , exception :: EXCEPTION | e) Unit
  -> Eff (console :: CONSOLE , exception :: EXCEPTION | e) Unit
yargsCommand command defaultFn fn = runY mempty $ go <$> (foreignToString <$> rest)
  where
    go :: Array String -> Eff (console :: CONSOLE , exception :: EXCEPTION | e) Unit
    go a = case head a of
      (Just entered) -> if entered == command then fn else pure unit
      _ -> defaultFn

-- | Given an array of foreign data, return an array of strings
foreignToString :: Array Foreign -> Array String
foreignToString a = a # foldMap (\v -> case runExcept (readString v) of
  Left _ -> []
  Right b -> [b]
)

-- | Serialize game content, showing either an error or a success message
saveGameEff :: forall e. To String -> String -> From String -> Eff (buffer :: BUFFER, console :: CONSOLE, fs :: FS | e) Unit
saveGameEff to template from =
  void $ runAff
    (\e -> EC.error $ message e )
    (\(FSResult r) -> log r)
    go

  where
  go = do
    let templateFolder = "templates/" <> template
    templateExists <- exists templateFolder
    case templateExists of 
      true -> do
        AC.log $ "Copying '" <> templateFolder <> "' to '" <> (unwrap to) <> "' ... "
        copyDir templateFolder (unwrap to)
        AC.log "Done."

        AC.log $ "Copying 'dev/main.js' to '" <> (unwrap to) <> "' ... "
        copyFile "dev/main.js" ((unwrap to) <> sep <> "main.js")
        AC.log "Done."

        AC.log "Parsing and serializing game..."
        saveGame from to
      false -> pure $ FSResult $ templateFolder <> " does not exist."     

-- | Serialize game content, showing either an error or a success message
saveAdventureGameEff :: forall e. To String -> String -> From String -> Eff (buffer :: BUFFER, console :: CONSOLE, fs :: FS | e) Unit
saveAdventureGameEff to template from =
  void $ runAff
    (\e -> EC.error $ message e )
    (\(FSResult r) -> log r)
    go

  where
  go = do
    let templateFolder = "templates/" <> template
    templateExists <- exists templateFolder
    case templateExists of 
      true -> do
        AC.log $ "Copying '" <> templateFolder <> "' to '" <> (unwrap to) <> "' ... "
        copyDir templateFolder (unwrap to)
        AC.log "Done."

        AC.log $ "Copying 'dev/main.js' to '" <> (unwrap to) <> "' ... "
        copyFile "dev/main.js" ((unwrap to) <> sep <> "main.js")
        AC.log "Done."

        AC.log "Parsing and serializing game..."
        saveAdventureGame from to
      false -> pure $ FSResult $ templateFolder <> " does not exist."     


data Template
  = Namaqua
  | Business

strToTemplate :: String -> Either Error Template
strToTemplate "namaqua" = Right Namaqua
strToTemplate "business" = Right Business
strToTemplate s = Left $ error $ s <> " is not a valid template"

adventureCompiler :: forall e. Eff (exception :: EXCEPTION, buffer :: BUFFER, console :: CONSOLE, fs :: FS | e) Unit
adventureCompiler = do
  runY mempty $ saveAdventureGameEff <$>
    To <$> (yarg "t" ["to"]
      (Just "Folder where story should be generated")
      (Right "You must specify the folder where the generated story will go.")
      true
    ) <*>
    (yarg "s" ["style"]
      (Just "Template to use (see folders in /templates)")
      (Left "simple")
      false
    ) <*>
    (From <$> (yarg "f" ["file"]
      (Just "Location of the choose-your-own-adventure .str file.")
      (Right "You must specify where the .str file is located.")
      true
    ))

showCommands :: forall e. Eff (console :: CONSOLE | e) Unit
showCommands = do
  log "Please enter a command. Available commands are:\n\n adventure"

main :: forall e. Eff (buffer :: BUFFER, console :: CONSOLE, fs :: FS, exception :: EXCEPTION| e) Unit
main = do
  yargsCommand "adventure" showCommands adventureCompiler


data MyCommands
  = Compile String
  | Check String
  | Upload String

derive instance genericMyCommands :: Generic MyCommands _

class MyCommandRunner a where
  genericRunCommand :: a -> String

instance b :: (IsSymbol name) => MyCommandRunner (Constructor name (Argument String)) where
  genericRunCommand (Constructor (Argument a)) =
    ("Constructor " <> name <> " : " <> a <> ")")
    where name = reflectSymbol (SProxy :: SProxy name)

instance c :: (MyCommandRunner a, MyCommandRunner b) => MyCommandRunner (Sum a b) where
  genericRunCommand (Inl a) = genericRunCommand a
  genericRunCommand (Inr b) = genericRunCommand b




  -- -- from $ Compile (YargCommand { name : "compile", description : "compile the project" })
  -- -- log $ genericRunCommand (from $ Check "test")

  -- let setup = usage "$0" <> example "$0"
  -- let compileSetup = usage "$0" <> example "$0"

  -- runY setup 
  -- -- runYCommand setup $ case _ of
  -- --   Compile -> app <$ (runY compileSetup)