module MyPrelude (
  module Exports
  ) where

import Prelude as Exports
import Control.Monad.Eff (Eff)  as Exports
import Control.Monad.Eff.Console (CONSOLE) as Exports
import Control.Monad.Eff.Now (NOW) as Exports
import Control.Monad.Eff.Random (RANDOM) as Exports
import Control.Monad.Eff.Ref (REF, Ref, newRef, readRef, modifyRef) as Exports
import DOM (DOM) as Exports
import Data.Foldable (foldl) as Exports
import Data.Generic (class Generic, gEq) as Exports
import Data.StrMap (StrMap, empty, union, fromFoldable) as Exports
import Data.Traversable (sequence) as Exports
import Data.Tuple (Tuple, Tuple(..)) as Exports
import Data.Maybe (maybe, Maybe(..), Maybe, fromMaybe, fromJust) as Exports
import Data.Array (head)