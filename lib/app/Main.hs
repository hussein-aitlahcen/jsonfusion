-- Main.hs ---

-- Copyright (C) 2018 Hussein Ait-Lahcen

-- Author: Hussein Ait-Lahcen <hussein.aitlahcen@gmail.com>

-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 3
-- of the License, or (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Monad.Except       (MonadError, MonadIO, liftIO,
                                             runExceptT, throwError)
import           Data.Aeson                 (eitherDecode, encode)
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Semigroup             ((<>))
import qualified Data.Vector                as V
import           System.Environment         (getArgs)
import           Types

type Arguments = (FilePath, FilePath, FilePath)
type Content   = BS.ByteString

brusselFeatureId :: FeatureId
brusselFeatureId = 54094

decodeFeature :: (MonadError String m) => Content -> m FeatureCollection
decodeFeature content = case eitherDecode content of
                          Right featureCollection -> pure featureCollection
                          Left msg                -> throwError msg

readAndMerge :: (MonadIO m, MonadError String m) => Arguments -> m ()
readAndMerge (provincesPath, regionsPath, outputPath) = do
  provincesContent <- liftIO . BS.readFile $ provincesPath
  regionsContent   <- liftIO . BS.readFile $ regionsPath
  provinces        <- decodeFeature provincesContent
  regions          <- decodeFeature regionsContent
  let regionFeatures    = features regions
      isBrusselFeature  = (==) brusselFeatureId . featureId
      filterBrusselOnly = V.filter isBrusselFeature
      brussel           = filterBrusselOnly regionFeatures
      patchedRegions    = regions { features = brussel }
      merged            = provinces <> patchedRegions
  liftIO . BS.writeFile outputPath . encode $ merged

parseArguments :: [String] -> Either String Arguments
parseArguments (provincesPath:regionPaths:outputFilePath:[]) =
  Right (provincesPath, regionPaths, outputFilePath)
parseArguments _ =
  Left "Only three arguments are allowed, please give them this way: <provincesFilePath> <regionsFilePath> <outputFilePath>"

main :: IO ()
main = do
  result <- runExceptT . mapM readAndMerge <$> parseArguments =<< getArgs
  case result of
    Right (Right _)       -> putStrLn "Sucessfully merged the two features."
    Right (Left argError) -> putStrLn $ "Arguments error: " ++ argError
    Left decodeError      -> putStrLn $ "Decoding error: " ++ decodeError

