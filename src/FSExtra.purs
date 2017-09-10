module FSExtra where
  
import Prelude

import Control.Monad.Aff (Aff)
import Data.Traversable (traverse)
import Node.Buffer (Buffer, BUFFER)
import Node.Encoding (Encoding(..))
import Node.FS (FS)
import Node.FS.Aff (exists, mkdir, readFile, readTextFile, readdir, rmdir, stat, unlink, writeFile, writeTextFile)
import Node.FS.Stats (isDirectory)
import Node.Path (FilePath, concat, sep)

-- mkdir throws if the directory already exists, safeMkdir doesn't
safeMkdir :: forall eff. FilePath -> Aff (fs :: FS | eff) Unit
safeMkdir f = exists f >>= (\doesExist -> if (not doesExist) then mkdir f else pure unit)

-- rmdir doesn't remove folders when they're not empty, rmdirRecur does
rmdirRecur :: forall eff. FilePath -> Aff (fs :: FS | eff) Unit
rmdirRecur d = do 
  exist <- exists d
  if (not exist)
    then pure unit
    else do
      filesToDelete <- readdir d
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

-- | Copy the contents of one directory into another, leaving
-- | the existing files and overwriting files that are in both
-- | source and target.
copyDir :: forall eff. FilePath -> FilePath -> Aff (fs :: FS, buffer :: BUFFER | eff) Unit
copyDir from to = do
  -- rmdir to
  filesToCopy <- readdir from
  safeMkdir to
  void $ traverse (\f -> do
    let from' = from <> sep <> f
    let to' = to <> sep <> f
    copyFile from' to'
  ) filesToCopy
  pure unit

copyFile  :: forall eff. FilePath -> FilePath -> Aff (fs :: FS, buffer :: BUFFER | eff) Unit
copyFile from to = do
  stats <- stat from
  case isDirectory stats of
    true -> pure unit
    false -> do
      b <- readFile from
      writeFile to b