module Time where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Now (NOW, nowDateTime)
import Data.DateTime (DateTime(..))
import Data.DateTime.Locale (LocalDateTime, LocalValue(..))
import Data.Enum (fromEnum)
import Data.Semigroup ((<>))
import Data.Time (Time(..))

now :: forall e. Eff (now :: NOW | e) LocalDateTime
now = nowDateTime

clockDisplay :: LocalDateTime -> String
clockDisplay (LocalValue local (DateTime date (Time hour minute second _))) =
  (show $ fromEnum hour) <> ":" <> (show $ fromEnum minute) <> ":" <> (show $ fromEnum second)

