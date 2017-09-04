module FSExtra where
  
import Prelude

import Control.Monad.Aff (Aff)
import Data.Traversable (traverse)
import Node.Buffer (Buffer, BUFFER)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Aff (exists, mkdir, readTextFile, readdir, rmdir, stat, unlink, writeFile, writeTextFile)
import Node.FS.Stats (isDirectory)
import Node.Path (FilePath, concat, sep)

-- mkdir throws if the directory already exists, safeMkdir doesn't
safeMkdir :: forall eff. FilePath -> Aff (fs :: FS | eff) Unit
safeMkdir f = exists f >>= (\doesExist -> if (not doesExist) then mkdir f else pure unit)

-- rmdir doesn't remove folders when they're not empty, rmdirRecur does
rmdirRecur :: forall eff. FilePath -> Aff (fs :: FS | eff) Unit
rmdirRecur d = do 
  exist <- exists d
  if (not exist) then pure unit
                 else do filesToDelete <- readdir d
                         void $ traverse (\f -> do
                                      let path = concat [d, f]
                                      s <- stat path
                                      if isDirectory s then rmdirRecur path else unlink path ) filesToDelete
                         rmdir d
  
overWriteFile :: forall eff. FilePath -> Buffer -> Aff (fs :: FS, buffer :: BUFFER | eff) Unit
overWriteFile f b = do
  exist <- exists f
  if (exist) then unlink f
             else pure unit
  writeFile f b 

copyDir :: forall eff. FilePath -> FilePath -> Aff (fs :: FS, buffer :: BUFFER | eff) Unit
copyDir from to = do
  rmdirRecur to
  filesToCopy <- readdir from
  mkdir to
  void $ traverse (\f -> do
    stats <- stat (from <> sep <> f)
    case isDirectory stats of
      true -> copyDir (from <> sep <> f) (to <> sep <> f)
      false -> do
        b <- readTextFile UTF8 (concat [from, sep, f])
        writeTextFile UTF8 (concat [to, sep, f]) b
  ) filesToCopy
  pure unit

copyFile  :: forall eff. FilePath -> FilePath -> Aff (fs :: FS, buffer :: BUFFER | eff) Unit
copyFile from to = do
  stats <- stat from
  case isDirectory stats of
    true -> pure unit
    false -> do
      b <- readTextFile UTF8 from
      writeTextFile UTF8 to b