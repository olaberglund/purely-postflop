{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad ((>=>))
import Data.Text (Text, pack, splitOn)
import Data.Text.IO (readFile)
import System.Directory.Recursive (getDirRecursive)
import System.Posix (getFileStatus)
import System.Posix.Files (isDirectory)
import Text.JSON
import Prelude hiding (readFile)

datas :: FilePath -> IO [PathData]
datas = getDirRecursive >=> traverse extract

main :: IO ()
main = datas "Cash6m50z100bbGeneral" >>= writeFile "test.json" . encode . addRoot

addRoot :: [PathData] -> JSValue
addRoot d = makeObj [("version", showJSON (2 :: Int)), ("name", "ranges"), ("data", showJSON d)]

extract :: FilePath -> IO PathData
extract fp = do
  stat <- getFileStatus fp
  if isDirectory stat
    then return $ Group (Path $ pack fp)
    else Range (Path $ pack fp) . Value <$> readFile fp

newtype Path = Path Text

newtype Value = Value Text

data PathData = Group !Path | Range !Path !Value

instance JSON PathData where
  showJSON :: PathData -> JSValue
  showJSON d =
    let baseJSON p b = [("path", showJSONs (splitOn "/" p)), ("isGroup", showJSON b)]
     in case d of
          Group (Path p) -> makeObj (baseJSON p True)
          (Range (Path p) (Value v)) -> makeObj $ ("value", showJSON v) : baseJSON p False

  -- don't need this, but let's make PathData a proper instance
  readJSON :: JSValue -> Result PathData
  readJSON val = case val of
    (JSObject o) -> do
      path <- valFromObj "path" o
      isGroup <- valFromObj "isGroup" o
      if isGroup
        then return $ Group (Path path)
        else Range (Path path) . Value <$> valFromObj "value" o
    _fail -> Error "Invalid Data"
