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

import           Control.Lens               (view, (%~), (&))
import           Control.Monad.Except       (runExceptT)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Vector                as V
import           JsonFusion                 (jsonFusion)
import           System.Environment         (getArgs)
import           Types

-- Could have use the ISO string aswell,
-- but we would have to project the inner 'property' object...
brusselFeatureId = 54094

main :: IO ()
main = do
  -- Yeah yeah we sould correctly parse our input arguments :P
  (provincesPath:regionsPath:beligiumPath:_)  <- getArgs
  provincesContent <- BS.readFile provincesPath
  regionsContent   <- BS.readFile regionsPath
  merged           <- runExceptT $ jsonFusion aggregate provincesContent regionsContent
  case merged of
    Left msg          -> putStrLn $ "Unlucky boy: " ++ show msg
    Right fullBelgium -> BS.writeFile beligiumPath fullBelgium
  where
    -- Folding behavior
    aggregate provinces regions =
      let regionFeatures    = view features regions
          isBrusselFeature  = (==) brusselFeatureId . view featureId
          filterBrusselOnly = V.filter isBrusselFeature
      in provinces & features %~ (V.++) (filterBrusselOnly regionFeatures)
