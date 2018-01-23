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

module Main where

import           Control.Monad.Except       (runExceptT)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Vector                as V
import           JsonFusion                 (FusionError, jsonFusion)
import           System.Environment         (getArgs)
import           Types

type Arguments = (FilePath, FilePath, FilePath)

brusselFeatureId :: FeatureId
brusselFeatureId = 54094

parseArguments :: [String] -> Either String Arguments
parseArguments (provincesPath:regionPaths:outputFilePath:[]) = Right (provincesPath, regionPaths, outputFilePath)
parseArguments _ = Left "Only two arguments are allowed, please give them this way: <provincesFilePath> <regionsFilePath> <outputFilePath>"

aggregate :: FeatureCollection -> FeatureCollection -> FeatureCollection
aggregate provinces regions = provinces { features = provincesFeatures V.++ brussel }
  where regionFeatures      = features regions
        isBrusselFeature    = (==) brusselFeatureId . featureId
        filterBrusselOnly   = V.filter isBrusselFeature
        brussel             = filterBrusselOnly regionFeatures
        provincesFeatures   = features provinces

readAndFusion :: Arguments -> IO (Either FusionError ())
readAndFusion (provincesPath, regionsPath, outputPath) = do
  provincesContent <- BS.readFile provincesPath
  regionsContent   <- BS.readFile regionsPath
  merged           <- runExceptT $ jsonFusion aggregate provincesContent regionsContent
  mapM (BS.writeFile outputPath) merged

main :: IO ()
main = do
  result <- mapM readAndFusion <$> parseArguments =<< getArgs
  case result of
    Right (Right _)          -> putStrLn "Sucessfully merged the two files"
    Right (Left fusionError) -> putStrLn $ "Fusion error: " ++ show fusionError
    Left msg                 -> putStrLn $ "Arguments error: " ++ msg
